/*
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/*
 This handles all of the score tiebreak logic. Implements Buchholz,
 Buchholz Cut 1, and Sonneborn-Berger tiebreakers.
 */

module Score: {
  /**
    This manages the floating-point sum of individual match scores.
    */
  module Sum: {
    type t
    let toFloat: t => float
    let toNumeral: t => Numeral.t
  }

  type t = Zero | One | NegOne | Half

  let fromResultWhite: Data_Match.Result.t => t

  let fromResultBlack: Data_Match.Result.t => t

  let sum: list<t> => Sum.t
  let calcScore: (list<t>, ~adjustment: float) => Sum.t
  let toFloat: t => float
}

module Color: {
  type t = White | Black

  let opposite: t => t

  /**
    This is used for tracking colors.
    */
  let toScore: t => Score.t
}

type t = {
  colorScores: list<Score.t>,
  lastColor: option<Color.t>, // This is used to create pairing data
  id: Data_Id.t,
  isDummy: bool,
  opponentResults: list<(Data_Id.t, Score.t)>,
  ratings: list<int>,
  firstRating: int,
  results: list<Score.t>,
  resultsNoByes: list<Score.t>,
  adjustment: float,
}

let oppResultsToSumById: (t, Data_Id.t) => option<Score.Sum.t>

module TieBreak: {
  /**
    These types are used in various parts of the rest of the app. They map to:

    - What tiebreak function to use.
    - What tiebreak value has been computed for a player.
    - What human-language name to display for the tiebreak.
    - How to encode or decode a reference to a tiebreak for JS.
    */
  type t =
    | Buchholz
    | BuchholzCut1
    | SonnebornBerger

  let encode: t => Js.Json.t

  let decode: Js.Json.t => t

  let toString: t => string

  let toPrettyString: t => string
}

let make: Data_Id.t => t

type scores = {
  id: Data_Id.t,
  score: Score.Sum.t,
  buchholz: Score.Sum.t,
  buchholzCut1: Score.Sum.t,
  sonnebornBerger: Score.Sum.t,
}

let getTieBreak: (scores, TieBreak.t) => Score.Sum.t

/**
  Sort the standings by score, using the specified tiebreak methods.
  Returns the list of the standings. Each standing has tiebreak scores
  for each method. The sorting order follows the order of methods provided.
  */
let createStandingArray: (Data_Id.Map.t<t>, array<TieBreak.t>) => array<scores>

/**
  Groups the standings by score and tiebreak values.
  example: `[[Dale, Audrey], [Pete], [Bob]]` Dale and Audrey are tied for
  first, Pete is 2nd, Bob is 3rd.
  */
let createStandingTree: (array<scores>, ~tieBreaks: array<TieBreak.t>) => list<list<scores>>

let fromTournament: (
  ~roundList: Data_Rounds.t,
  ~scoreAdjustments: Data_Id.Map.t<float>,
) => Data_Id.Map.t<t>