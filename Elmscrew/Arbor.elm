port module Elmscrew.Arbor exposing (..)

import Elmscrew.Parser exposing (Inst(..))

port displayGraph : List (String, List Int) -> Cmd msg
