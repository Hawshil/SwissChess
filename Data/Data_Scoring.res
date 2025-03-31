/*
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open Belt
module Id = Data_Id

module Score = {
  module Sum: {
  type t
  let zero: t
  let add: (t, t) => t
  let compare: (t, t) => int
  let eq: (t, t) => bool
  let sum: list<t> => t
  let fromFloat: float => t
  let toFloat: t => float
  let toNumeral: t => Numeral.t
  let calcScore: (list<t>, ~adjustment: float) => t
} = {
    type t = float
    let zero = 0.0
    let add = (a, b) => a +. b
    let compare: (t, t) => int = compare
    let eq: (t, t) => bool = (a, b) => a == b
    let sum = list => List.reduce(list, zero, add)
    let fromFloat = x => x
    let toFloat = x => x
    let toNumeral = Numeral.make
    let calcScore = (results, ~adjustment) => add(sum(results), fromFloat(adjustment))
  }

  type t = Zero | One | NegOne | Half

  let toFloat = x =>
    switch x {
    | Zero => 0.0
    | One => 1.0
    | NegOne => -1.0
    | Half => 0.5
    }

  let toSum = x => x->toFloat->Sum.fromFloat

  let add = (a, b) => Sum.add(a, toSum(b))
  let sum = list => List.reduce(list, Sum.zero, add)
  // let calcScore = (results, ~adjustment) => Sum.add(sum(results), Sum.fromFloat(adjustment))
  let calcScore = (results, ~adjustment) => Sum.calcScore(List.map(results, toSum), ~adjustment)

  let fromResultWhite = (x: Data_Match.Result.t) =>
    switch x {
    | Draw => Half
    | WhiteWon | BlackAborted => One
    | BlackWon | WhiteAborted => Zero
    | NotSet | Aborted => Zero
    }

  let fromResultBlack = (x: Data_Match.Result.t) =>
    switch x {
    | Draw => Half
    | WhiteWon | BlackAborted => Zero
    | BlackWon | WhiteAborted => One
    | NotSet | Aborted => Zero
    }
}

module Color = {
  type t =
    | White
    | Black

  let opposite = x =>
    switch x {
    | White => Black
    | Black => White
    }

  let toScore = (x): Score.t =>
    switch x {
    | White => NegOne
    | Black => One
    }
}

type t = {
  colorScores: list<Score.t>,
  lastColor: option<Color.t>, // This is used to create pairing data
  id: Id.t,
  isDummy: bool,
  opponentResults: list<(Id.t, Score.t)>,
  ratings: list<int>,
  firstRating: int,
  results: list<Score.t>,
  resultsNoByes: list<Score.t>,
  adjustment: float,
}

let oppResultsToSumById = ({opponentResults, _}, id) =>
  List.reduce(opponentResults, None, (acc, (id', result)) =>
    if Id.eq(id, id') {
      switch acc {
      | Some(acc) => Some(Score.add(acc, result))
      | None => Some(Score.add(Score.Sum.zero, result))
      }
    } else {
      acc
    }
  )

module TieBreak = {
  type t =
    | Buchholz
    | BuchholzCut1
    | SonnebornBerger

  let toString = data =>
    switch data {
    | Buchholz => "buchholz"
    | BuchholzCut1 => "buchholzCut1"
    | SonnebornBerger => "sonnebornBerger"
    }

  let toPrettyString = tieBreak =>
    switch tieBreak {
    | Buchholz => "Buchholz"
    | BuchholzCut1 => "Buchholz cut 1"
    | SonnebornBerger => "Sonneborn-Berger"
    }

  let fromString = json =>
    switch json {
    | "buchholz" => Buchholz
    | "buchholzCut1" => BuchholzCut1
    | "sonnebornBerger" => SonnebornBerger
    // Default to Buchholz for backward compatibility
    | _ => Buchholz
    }

  let encode = data => data->toString->Js.Json.string

  @raises(Not_found)
  let decode = json => Js.Json.decodeString(json)->Option.getExn->fromString
}

let make = id => {
  colorScores: list{},
  lastColor: None,
  id,
  isDummy: false,
  opponentResults: list{},
  ratings: list{},
  firstRating: 0,
  results: list{},
  resultsNoByes: list{},
  adjustment: 0.0,
}

let isNotDummy = (scores, oppId) =>
  switch Map.get(scores, oppId) {
  | None => true
  | Some(opponent) => !opponent.isDummy
  }

let getPlayerScore = (scores, id) =>
  switch Map.get(scores, id) {
  | None => Score.Sum.zero
  | Some({results, adjustment, _}) => Score.calcScore(results, ~adjustment)
  }

let getOpponentScores = (scores, id) =>
  switch Map.get(scores, id) {
  | None => list{}
  | Some({opponentResults, _}) =>
    List.keepMap(opponentResults, ((oppId, _)) =>
      isNotDummy(scores, oppId) ? Some(getPlayerScore(scores, oppId)) : None
    )
  }

/**
  Buchholz: Sum of the tournament scores of all opponents.
  */
let getBuchholzScore = (scores, id) => scores->getOpponentScores(id)->Score.Sum.sum

/**
  Buchholz cut 1: Sum of the tournament scores of all opponents, except the one with the lowest score.
  */
let getBuchholzCut1Score = (scores, id) => {
  let oppScores = scores->getOpponentScores(id)
  let size = List.size(oppScores)
  
  if size <= 1 {
    Score.Sum.sum(oppScores)
  } else {
    oppScores
    ->List.sort(Score.Sum.compare)
    ->List.tailExn // Remove the lowest score
    ->Score.Sum.sum
  }
}

/**
  Sonneborn-Berger: Sum of the scores of defeated opponents plus half the sum of scores of drawn opponents.
  */
let getSonnebornBergerScore = (scores, id) =>
  switch Map.get(scores, id) {
  | None => Score.Sum.zero
  | Some({opponentResults, _}) =>
    opponentResults
    ->List.keepMap(((oppId, result)) => {
      if !isNotDummy(scores, oppId) {
        None
      } else {
        let oppScore = getPlayerScore(scores, oppId)
        switch result {
        | Score.One => Some(oppScore) // Full points for defeated opponents
        | Score.Half => Some(Score.Sum.fromFloat(Score.Sum.toFloat(oppScore) *. 0.5)) // Half points for drawn opponents
        | Score.Zero | Score.NegOne => None // No points for other results
        }
      }
    })
    ->Score.Sum.sum
  }

type scores = {
  id: Data_Id.t,
  score: Score.Sum.t,
  buchholz: Score.Sum.t,
  buchholzCut1: Score.Sum.t,
  sonnebornBerger: Score.Sum.t,
}

let getTieBreak = (scores, x: TieBreak.t) =>
  switch x {
  | Buchholz => scores.buchholz
  | BuchholzCut1 => scores.buchholzCut1
  | SonnebornBerger => scores.sonnebornBerger
  }

/**
  `a` and `b` have a list of tiebreak results. `tieBreaks` is a list of what
  tiebreak results to sort by, and in what order. It is expected that `a` and
  b` will have a result for every item in `tieBreaks`.
  */
let standingsSorter = (orderedMethods, a, b) => {
  let rec tieBreaksCompare = i =>
    switch orderedMethods[i] {
    | None => 0
    | Some(tieBreak) =>
      switch (getTieBreak(a, tieBreak), getTieBreak(b, tieBreak)) {
      | (a', b') =>
        /* a and b are switched for ascending order */
        switch Score.Sum.compare(b', a') {
        | 0 => tieBreaksCompare(succ(i))
        | x => x
        }
      }
    }
  /* a and b are switched for ascending order */
  switch Score.Sum.compare(b.score, a.score) {
  | 0 => tieBreaksCompare(0)
  | x => x
  }
}

let createStandingArray = (t, orderedMethods) =>
  t
  // Tiebreaks are computed even if they aren't necessary.
  // If this is a performance problem, they could be wrapped in a lazy type.
  ->Map.map(({id, results, adjustment, _}) => {
    id,
    score: Score.calcScore(results, ~adjustment),
    buchholz: getBuchholzScore(t, id),
    buchholzCut1: getBuchholzCut1Score(t, id),
    sonnebornBerger: getSonnebornBergerScore(t, id),
  })
  ->Map.valuesToArray
  ->SortArray.stableSortBy(standingsSorter(orderedMethods))

let eq = (a, b, tieBreaks) =>
  Score.Sum.eq(a.score, b.score) &&
  Array.every(tieBreaks, tb => Score.Sum.eq(getTieBreak(a, tb), getTieBreak(b, tb)))

let createStandingTree = (standingArray, ~tieBreaks) =>
  Array.reduce(standingArray, list{}, (tree, standing) =>
    switch tree {
    /* Always make a new rank for the first player */
    | list{} => list{list{standing}}
    | list{treeHead, ...treeTail} =>
      switch treeHead {
      | list{} => list{list{standing}, ...tree}
      /* Make a new rank if the scores aren't equal */
      | list{lastStanding, ..._} if !eq(lastStanding, standing, tieBreaks) =>
        list{list{standing}, treeHead, ...treeTail}
      | _ => list{list{standing, ...treeHead}, ...treeTail}
      }
    }
  )

let update = (
  data,
  ~playerId,
  ~origRating,
  ~newRating,
  ~result,
  ~oppId,
  ~color,
  ~scoreAdjustments,
) =>
  switch data {
  | None =>
    Some({
      id: playerId,
      firstRating: origRating,
      adjustment: Map.getWithDefault(scoreAdjustments, playerId, 0.0),
      results: list{result},
      resultsNoByes: Data_Id.isDummy(oppId) ? list{} : list{result},
      lastColor: Some(color),
      colorScores: list{Color.toScore(color)},
      opponentResults: list{(oppId, result)},
      ratings: list{newRating},
      isDummy: Data_Id.isDummy(playerId),
    })
  | Some(data) =>
    Some({
      ...data,
      results: list{result, ...data.results},
      resultsNoByes: Data_Id.isDummy(oppId)
        ? data.resultsNoByes
        : list{result, ...data.resultsNoByes},
      lastColor: Some(color),
      colorScores: list{Color.toScore(color), ...data.colorScores},
      opponentResults: list{(oppId, result), ...data.opponentResults},
      ratings: list{newRating, ...data.ratings},
    })
  }

let fromTournament = (~roundList, ~scoreAdjustments) =>
  roundList
  ->Data_Rounds.rounds2Matches
  ->MutableQueue.reduce(Map.make(~id=Data_Id.id), (acc, match: Data_Match.t) =>
    switch match.result {
    | NotSet => acc
    | WhiteWon | BlackWon | Draw | Aborted | WhiteAborted | BlackAborted =>
      let whiteUpdate = update(
        ~playerId=match.whiteId,
        ~origRating=match.whiteOrigRating,
        ~newRating=match.whiteNewRating,
        ~result=Score.fromResultWhite(match.result),
        ~oppId=match.blackId,
        ~color=White,
        ~scoreAdjustments,
      )
      let blackUpdate = update(
        ~playerId=match.blackId,
        ~origRating=match.blackOrigRating,
        ~newRating=match.blackNewRating,
        ~result=Score.fromResultBlack(match.result),
        ~oppId=match.whiteId,
        ~color=Black,
        ~scoreAdjustments,
      )
      acc->Map.update(match.whiteId, whiteUpdate)->Map.update(match.blackId, blackUpdate)
    }
  )