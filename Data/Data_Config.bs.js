// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Js_json from "rescript/lib/es6/js_json.js";
import * as Belt_Set from "rescript/lib/es6/belt_Set.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Data_Id$Coronate from "./Data_Id.bs.js";

function toFloat(x) {
  switch (x) {
    case /* Full */0 :
        return 1.0;
    case /* Half */1 :
        return 0.5;
    case /* Zero */2 :
        return 0.0;
    
  }
}

function fromFloat(x) {
  if (x !== 0.0) {
    if (x !== 0.5) {
      return /* Full */0;
    } else {
      return /* Half */1;
    }
  } else {
    return /* Zero */2;
  }
}

var encode = toFloat;

function decode(json) {
  return fromFloat(Belt_Option.getExn(Js_json.decodeNumber(json)));
}

var ByeValue = {
  toFloat: toFloat,
  fromFloat: fromFloat,
  encode: encode,
  decode: decode
};

function decode$1(json) {
  var d = Belt_Option.getExn(Js_json.decodeObject(json));
  var json$1 = Belt_Option.getExn(Js_dict.get(d, "byeValue"));
  return {
          avoidPairs: Curry._1(Data_Id$Coronate.Pair.$$Set.decode, Belt_Option.getExn(Js_dict.get(d, "avoidPairs"))),
          byeValue: fromFloat(Belt_Option.getExn(Js_json.decodeNumber(json$1))),
          lastBackup: new Date(Belt_Option.getExn(Belt_Option.flatMap(Js_dict.get(d, "lastBackup"), Js_json.decodeString))),
          whiteAlias: Belt_Option.flatMap(Js_dict.get(d, "whiteAlias"), Js_json.decodeString),
          blackAlias: Belt_Option.flatMap(Js_dict.get(d, "blackAlias"), Js_json.decodeString)
        };
}

function encode$1(data) {
  var o = data.whiteAlias;
  var o$1 = data.blackAlias;
  return Js_dict.fromArray([
              [
                "avoidPairs",
                Curry._1(Data_Id$Coronate.Pair.$$Set.encode, data.avoidPairs)
              ],
              [
                "byeValue",
                toFloat(data.byeValue)
              ],
              [
                "lastBackup",
                data.lastBackup.toJSON()
              ],
              [
                "whiteAlias",
                o !== undefined ? o : null
              ],
              [
                "blackAlias",
                o$1 !== undefined ? o$1 : null
              ]
            ]);
}

var default_avoidPairs = Belt_Set.make(Data_Id$Coronate.Pair.id);

var default_lastBackup = new Date(0.0);

var $$default = {
  avoidPairs: default_avoidPairs,
  byeValue: /* Full */0,
  lastBackup: default_lastBackup,
  whiteAlias: undefined,
  blackAlias: undefined
};

function alias(s) {
  if (s === "") {
    return ;
  } else {
    return s;
  }
}

function aliasToStringWhite(t) {
  var s = t.whiteAlias;
  if (s !== undefined) {
    return s;
  } else {
    return "White";
  }
}

function aliasToStringBlack(t) {
  var s = t.blackAlias;
  if (s !== undefined) {
    return s;
  } else {
    return "Black";
  }
}

function aliasToOption(o) {
  return o;
}

var aliasEmpty;

export {
  ByeValue ,
  decode$1 as decode,
  encode$1 as encode,
  $$default ,
  $$default as default,
  aliasEmpty ,
  alias ,
  aliasToStringWhite ,
  aliasToStringBlack ,
  aliasToOption ,
}
/* default Not a pure module */
