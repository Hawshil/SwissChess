/*
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module EloRank: {
  type t = int
  let getKFactor: (~matchCount: Data_Player.NatInt.t, ~rating: int) => int
}

/**
  Make new ratings for white and black.
  */
let calcNewRatings: (
  ~whiteRating: EloRank.t,
  ~blackRating: EloRank.t,
  ~whiteMatchCount: Data_Player.NatInt.t,
  ~blackMatchCount: Data_Player.NatInt.t,
  ~result: Data_Match.Result.t,
) => (EloRank.t, EloRank.t)
