port module Elmscrew.Vis exposing (..)

import Json.Encode

port displayGraph : (Json.Encode.Value, Json.Encode.Value) -> Cmd msg
port sendCurrentNode : String -> Cmd msg
