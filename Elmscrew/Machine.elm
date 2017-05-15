module Elmscrew.Machine exposing (Machine, empty, get, set, incr, decr, left, right)

import Dict exposing (Dict, get)

type alias Machine = {tape: Dict Int Int, position: Int}

empty = Machine (Dict.empty) 0

get : Machine -> Int
get machine   = Maybe.withDefault 0 <| Dict.get machine.position machine.tape

set : Machine -> Int -> Machine
set machine i = Machine (Dict.insert machine.position i machine.tape) machine.position

incr machine  = set machine <| 1 + get machine
decr machine  = set machine <| -1 + get machine

right machine = Machine machine.tape <| machine.position + 1
left machine = Machine machine.tape <| machine.position - 1
