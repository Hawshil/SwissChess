// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Js_json from "rescript/lib/es6/js_json.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function decode(json) {
  var d = Belt_Option.getExn(Js_json.decodeObject(json));
  return {
          github_token: Belt_Option.getExn(Belt_Option.flatMap(Js_dict.get(d, "github_token"), Js_json.decodeString)),
          github_gist_id: Belt_Option.getExn(Belt_Option.flatMap(Js_dict.get(d, "github_gist_id"), Js_json.decodeString))
        };
}

function encode(data) {
  return Js_dict.fromArray([
              [
                "github_token",
                data.github_token
              ],
              [
                "github_gist_id",
                data.github_gist_id
              ]
            ]);
}

var $$default = {
  github_token: "",
  github_gist_id: ""
};

export {
  encode ,
  decode ,
  $$default ,
  $$default as default,
}
/* No side effect */
