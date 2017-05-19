module Elmscrew.Interpreter exposing (Interpreter,init,initWithStr,initWithProg,step,runToCompletion,Status(..),execute)

import Elmscrew.Parser exposing (..)
import Elmscrew.Machine as Machine exposing (..)
import Elmscrew.Utils exposing (..)
import Elmscrew.Instruction exposing (..)

import Array exposing (Array,get)
import Char exposing (..)

type alias Interpreter =
    { instructions : Array Inst
    , machine : Machine
    , pc : Int
    , output : Maybe Char
    }

init = Interpreter Array.empty Machine.init 0 Nothing
initWithStr str = initWithProg <| parse str
initWithProg prog = Interpreter prog Machine.init 0 Nothing

type Status = Running Interpreter String (Maybe Char) | Complete Interpreter

execute : Interpreter -> Inst -> String -> (Interpreter, String, Maybe Char)
execute interp inst input =
    case inst of
        Right ->
            ({ interp | machine = right interp.machine }, input, Nothing)

        Left ->
            ({ interp | machine = left interp.machine  }, input, Nothing)

        Inc ->
            ({ interp | machine = incr interp.machine  }, input, Nothing)

        Dec ->
            ({ interp | machine = decr interp.machine  }, input, Nothing)

        Output ->
            (interp, input, (Just <| Char.fromCode <| Machine.get interp.machine))

        Input -> case String.uncons input of
                     Just (head, tail) ->
                         ({ interp | machine = Machine.set interp.machine (Char.toCode head) }, tail, Nothing)
                     Nothing ->
                         Debug.crash "Not enough input"
        JumpMarker -> (interp, input, Nothing)
        (Jump i) ->
            ({ interp | pc = if Machine.get interp.machine > 0 then i else interp.pc + 1}, input, Nothing)


step : Interpreter -> String -> Status
step interp input = let inst = Array.get interp.pc interp.instructions in
                    case inst of
                        Just x -> let (newInterp, newInput, newOutput) = execute interp x input in
                                  case x of
                                      (Jump i) -> Running newInterp newInput newOutput
                                      x -> Running { newInterp | pc = newInterp.pc + 1 } newInput newOutput
                        Nothing -> Complete interp

runToCompletion output interp input =
    case step interp input of
        Running newInterp newInput char ->
            let newOutput = case char of
                                Just c -> (String.cons c output)
                                Nothing -> output
            in
            runToCompletion newOutput newInterp newInput
        Complete newInterp -> (newInterp, String.reverse output)
