port module Elmscrew.Arbor exposing (..)

import Json.Encode

port displayGraph : (Json.Encode.Value, Json.Encode.Value) -> Cmd msg
