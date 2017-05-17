module Elmscrew.Interpreter exposing (Interpreter, initWithStr, initWithProg, step, runToCompletion, Status(..))

import Elmscrew.Parser exposing (..)
import Elmscrew.Machine as Machine exposing (..)
import Elmscrew.Utils exposing (..)

import Array exposing (Array,get)
import Char exposing (..)

type alias Interpreter a =
    { instructions : Array Inst
    , machine : Machine
    , pc : Int
    , inputProvider : (a -> Char)
    , output : Maybe Char
    }

initWithStr inputProvider str = initWithProg inputProvider <| parse str
initWithProg inputProvider prog = Interpreter prog Machine.init 0 inputProvider Nothing                         

type Status a = Running (Interpreter a) (Maybe Char) | Complete (Interpreter a)

step data interp =
    case (Array.get interp.pc interp.instructions) of
        Just Right ->
            Running { interp
                        | machine = right interp.machine
                        , pc = interp.pc + 1
                    } Nothing
       
        Just Left ->
            Running { interp
                        | machine = left interp.machine
                        , pc = interp.pc + 1
                    } Nothing

        Just Inc ->
            Running { interp
                       | machine = incr interp.machine
                       , pc = interp.pc + 1
                    } Nothing 

        Just Dec  ->
            Running { interp
                        | machine = decr interp.machine
                        , pc = interp.pc + 1
                    } Nothing

        Just Output ->
            Running { interp | pc = interp.pc + 1 }
                    (Just <| Char.fromCode <| Machine.get interp.machine)
                
        Just Input -> Running { interp | pc = interp.pc + 1 } Nothing
        Just JumpMarker -> Running { interp | pc = interp.pc + 1 } Nothing
        Just (Jump i) ->
            Running { interp | pc = if Machine.get interp.machine > 0 then i else interp.pc + 1} Nothing
                          
        Nothing -> Complete interp

runToCompletion data output interp =
    case step data interp of
        Running newInterp char ->
            let newOutput = case char of
                                Just c -> (String.cons c output)
                                Nothing -> output
            in
            runToCompletion data newOutput newInterp
        Complete newInterp -> (newInterp, String.reverse output)
