/*
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Type: {
  type t = Person | Dummy | Missing

  let toString: t => string
}

module NatInt: {
  /** A natural (non-negative) integer. */
  type t
  let fromInt: int => t
  let toInt: t => int
  let toString: t => string
}

type t = {
  firstName: string,
  id: Data_Id.t,
  lastName: string,
  matchCount: NatInt.t,
  rating: int,
  type_: Type.t,
}

let fullName: t => string

let compareName: (t, t) => int

let succMatchCount: t => t
let predMatchCount: t => t

let setRating: (t, int) => t

let encode: t => Js.Json.t

let decode: Js.Json.t => t

/**
  This is the dummy profile that `getPlayerMaybe()` returns for bye rounds.
  */
let dummy: t

/**
 This function should always be used in components that *might* not be able to
 display current player information. This includes bye rounds with "dummy"
 players, or scoreboards where a player may have been deleted.
 */
let getMaybe: (Data_Id.Map.t<t>, Data_Id.t) => t
