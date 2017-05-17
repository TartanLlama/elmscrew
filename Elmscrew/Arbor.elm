port module Elmscrew.Arbor exposing (..)

import Elmscrew.Parser exposing (Inst(..))
import Json.Encode

port displayGraph : (Json.Encode.Value, Json.Encode.Value) -> Cmd msg
