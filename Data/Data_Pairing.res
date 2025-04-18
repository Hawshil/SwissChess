/*
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open! Belt
module Id = Data_Id

@deriving(accessors)
type player = {
  id: Id.t,
  avoidIds: Id.Set.t,
  colorScore: float,
  lastColor: option<Data_Scoring.Color.t>,
  rating: int,
  opponents: list<Id.t>,
  score: float,
}

@deriving(accessors)
type t = {
  players: Id.Map.t<player>,
  maxScore: float,
  maxPriority: float,
}

// Using the original descendingRating function as it was in the original code
// let descendingRating = Utils.descend(compare, (. x) => x.rating)

/*
 This function is simplified to remove half-related logic
 Simply prepares players for pairing based on score and rating
*/
let preparePlayers = data => {
  Map.map(data, playerData => playerData)
}

let priority = (~diffDueColor, ~scoreDiff, ~canMeet, ~maxScore) => {
  /*
  The weight given to match players with opposite due colors.
  (USCF § 27A4 and § 27A5)
 */
  let colors = diffDueColor ? 2. : 0.
  
  /* The weight given to match players with equal scores. (USCF § 27A2) */
  let scores = maxScore *. 16. -. scoreDiff *. 16.
  
  /*
   The weight given to avoid players meeting twice. This same weight is given to
   avoid matching players on each other's "avoid" list.
   This is the highest priority. (USCF § 27A1)
 */
  let canMeet = canMeet ? 32. *. maxScore : 0.
  colors +. scores +. canMeet
}

let calcMaxPriority = maxScore => {
  priority(~diffDueColor=true, ~scoreDiff=0., ~canMeet=true, ~maxScore)
}

let calcMaxScore = m => Map.reduce(m, 0., (acc, _, p) => max(acc, p.score))

let make = (scoreData, playerData, avoidPairs) => {
  let avoidMap = Data_Id.Pair.Set.toMap(avoidPairs)
  let players = Map.mapWithKey(playerData, (key, data: Data_Player.t) => {
    let playerStats = switch Map.get(scoreData, key) {
    | None => Data_Scoring.make(key)
    | Some(x) => x
    }
    let newAvoidIds = switch Map.get(avoidMap, key) {
    | None => Set.make(~id=Data_Id.id)
    | Some(x) => x
    }
    {
      avoidIds: newAvoidIds,
      colorScore: playerStats.colorScores->Data_Scoring.Score.sum->Data_Scoring.Score.Sum.toFloat,
      lastColor: playerStats.lastColor,
      id: data.id,
      opponents: playerStats.opponentResults->List.map(((id, _)) => id),
      rating: data.rating,
      score: playerStats.results
      ->Data_Scoring.Score.calcScore(~adjustment=playerStats.adjustment)
      ->Data_Scoring.Score.Sum.toFloat,
    }
  })->preparePlayers
  let maxScore = calcMaxScore(players)
  {players, maxScore, maxPriority: calcMaxPriority(maxScore)}
}

let keep = ({players, _}, ~f) => {
  let players = Map.keep(players, (key, player) => f(key, player))
  let maxScore = calcMaxScore(players)
  {players, maxScore, maxPriority: calcMaxPriority(maxScore)}
}

let calcPairIdeal = (player1, player2, ~maxScore) =>
  if Id.eq(player1.id, player2.id) {
    0.0
  } else {
    let metBefore = List.some(player1.opponents, id => Id.eq(player2.id, id))
    let mustAvoid = Set.has(player1.avoidIds, player2.id)
    let canMeet = !metBefore && !mustAvoid
    let diffDueColor = switch (player1.lastColor, player2.lastColor) {
    | (Some(color1), Some(color2)) => color1 != color2
    | (_, _) => true
    }
    let scoreDiff = abs_float(player1.score -. player2.score)
    priority(~diffDueColor, ~scoreDiff, ~maxScore, ~canMeet)
  }

let calcPairIdealByIds = ({players, maxScore, _}, p1, p2) =>
  switch (Map.get(players, p1), Map.get(players, p2)) {
  | (Some(p1), Some(p2)) => Some(calcPairIdeal(p1, p2, ~maxScore))
  | _ => None
  }

let sortByScoreThenRating = (data1, data2) =>
  switch compare(data1.score, data2.score) {
  | 0 => compare(data1.rating, data2.rating)
  | x => x
  }

let setByePlayer = (byeQueue, dummyId, data: t) => {
  let hasNotHadBye = p => !List.some(p.opponents, id => Id.eq(dummyId, id))
  /* if the list is even, just return it. */
  switch mod(Map.size(data.players), 2) {
  | exception Division_by_zero => (data, None)
  | 0 => (data, None)
  | _ =>
    let dataArr =
      data.players
      ->Map.valuesToArray
      ->Array.keep(hasNotHadBye)
      ->SortArray.stableSortBy(sortByScoreThenRating)
    let playerIdsWithoutByes = Array.map(dataArr, p => p.id)
    let hasntHadByeFn = id => Array.some(playerIdsWithoutByes, byeId => Id.eq(id, byeId))
    let nextByeSignups = Array.keep(byeQueue, hasntHadByeFn)
    let dataForNextBye = switch nextByeSignups[0] {
    /* Assign the bye to the next person who signed up. */
    | Some(id) =>
      switch Map.get(data.players, id) {
      | Some(_) as x => x
      | None => dataArr[0]
      }
    | None =>
      /* Assign a bye to the lowest-rated player in the lowest score group.
         Because the list is sorted, the last player is the lowest.
         (USCF § 29L2.) */
      switch dataArr[0] {
      | Some(_) as x => x
      /* In the impossible situation that *everyone* has played a bye
       round previously, then just pick the last player. */
      | None =>
        data.players->Map.valuesToArray->SortArray.stableSortBy(sortByScoreThenRating)->Array.get(0)
      }
    }
    let players = switch dataForNextBye {
    | Some(dataForNextBye) => Map.remove(data.players, dataForNextBye.id)
    | None => data.players
    }
    ({...data, players}, dataForNextBye)
  }
}

let assignColorsForPair = ((player1, player2)) =>
  /* This is a quick-and-dirty heuristic to keep color balances
     mostly equal. Ideally, it would also examine due colors and how
     many times a player played each color last. */
  if player1.colorScore < player2.colorScore {
    /* player 1 has played as white more than player 2 */
    (player2.id, player1.id)
  } else {
    /* player 1 has played as black more than player 2
     (or they're equal). */
    (player1.id, player2.id)
  }

let netScore = ((player1, player2)) => player1.score +. player2.score
let netRating = ((player1, player2)) => player1.rating + player2.rating

let sortByNetScoreThenRating = (pair1, pair2) =>
  switch compare(netScore(pair2), netScore(pair1)) {
  | 0 => compare(netRating(pair2), netRating(pair1))
  | x => x
  }

module IdMatch = unpack(Blossom.Match.comparable(Id.compare))

/* This is not optimized for performance, but in practice that hasn't been a
 problem yet. */
let pairPlayers = ({players, maxScore, _}) => {
  Map.reduce(players, list{}, (acc, p1Id, p1) =>
    Map.reduce(players, acc, (acc2, p2Id, p2) => list{
      (p1Id, p2Id, calcPairIdeal(p1, p2, ~maxScore)),
      ...acc2,
    })
  )
  /* Feed all of the potential matches to the Blossom algorithim and let the
   algorithm work its magic. */
  ->Blossom.Match.make(~id=module(IdMatch))
  /* Blossom returns redundant pair data. This filters them out. */
  ->Blossom.Match.reduce(~init=Set.make(~id=Data_Id.Pair.id), ~f=(acc, p1, p2) =>
    switch Data_Id.Pair.make(p1, p2) {
    | None => acc
    | Some(pair) => Set.add(acc, pair)
    }
  )
  /* Convert the ids back to their pairing data */
  ->Set.toArray
  ->Array.keepMap(pair => {
    let (p1, p2) = Data_Id.Pair.toTuple(pair)
    switch (Map.get(players, p1), Map.get(players, p2)) {
    | (Some(p1), Some(p2)) => Some((p1, p2))
    | _ => None
    }
  })
  ->SortArray.stableSortBy(sortByNetScoreThenRating)
  ->Array.map(assignColorsForPair)
}