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

type Status = Running Interpreter (Maybe Char) | Complete Interpreter

execute : Interpreter -> Inst -> (Interpreter, Maybe Char)
execute interp inst =
    case inst of 
        Right ->
            ({ interp | machine = right interp.machine }, Nothing)
       
        Left ->
            ({ interp | machine = left interp.machine  }, Nothing)

        Inc ->
            ({ interp | machine = incr interp.machine  }, Nothing)

        Dec ->
            ({ interp | machine = decr interp.machine  }, Nothing)

        Output ->
            (interp, (Just <| Char.fromCode <| Machine.get interp.machine))
                
        Input -> (interp, Nothing) --TODO
        JumpMarker -> (interp, Nothing)
        (Jump i) ->
            ({ interp | pc = if Machine.get interp.machine > 0 then i else interp.pc + 1}, Nothing)
          
    
step : Interpreter -> Status
step interp = let inst = Array.get interp.pc interp.instructions in
              case inst of
                  Just x -> let (newInterp, newOutput) = execute interp x in
                            case x of
                                (Jump i) -> Running newInterp newOutput
                                x -> Running { newInterp | pc = newInterp.pc + 1 } newOutput
                  Nothing -> Complete interp

runToCompletion output interp =
    case step interp of
        Running newInterp char ->
            let newOutput = case char of
                                Just c -> (String.cons c output)
                                Nothing -> output
            in
            runToCompletion newOutput newInterp
        Complete newInterp -> (newInterp, String.reverse output)
