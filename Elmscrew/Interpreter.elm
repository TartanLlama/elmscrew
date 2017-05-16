module Elmscrew.Interpreter exposing (Interpreter, init, step)

import Elmscrew.Parser exposing (..)
import Elmscrew.Machine as Machine exposing (..)
import Elmscrew.Utils exposing (..)

import Array exposing (Array,get)

type alias Interpreter =
    { instructions : Array Inst
    , machine : Machine
    , pc : Int
    }

init str = Interpreter (parse str) Machine.init 0

step interp =
    case unwrap "No such instruction" (Array.get interp.pc interp.instructions) of
        Right -> { interp
                     | machine = right interp.machine
                     , pc = interp.pc + 1
                 }
       
        Left -> { interp
                    | machine = left interp.machine
                    , pc = interp.pc + 1
                }

        Inc  -> { interp
                    | machine = incr interp.machine
                    , pc = interp.pc + 1
                }

        Dec  -> { interp
                     | machine = decr interp.machine
                     , pc = interp.pc + 1
                }

        Output -> interp
        Input -> interp
        Loop i -> interp

           
                  
