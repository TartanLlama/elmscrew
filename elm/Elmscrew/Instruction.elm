module Elmscrew.Instruction exposing (toChar, toInst, Inst(..))

import Elmscrew.Utils exposing (unwrap)

type Inst = Right | Left | Inc | Dec | Output | Input | JumpMarker | Jump Int

toChar : Inst -> Char
toChar inst = case inst of
                  Right -> '>' 
                  Left -> '<' 
                  Inc -> '+' 
                  Dec -> '-' 
                  Output -> '.' 
                  Input -> ','
                  JumpMarker -> '['                            
                  Jump i -> ']'
                  
                  
    
toInst : Int -> List Int -> Char -> (Maybe Inst, List Int, Int)
toInst charIdx stack c = case c of
               '>' -> (Just Right, stack, charIdx+1)
               '<' -> (Just Left, stack, charIdx+1)
               '+' -> (Just Inc, stack, charIdx+1)
               '-' -> (Just Dec, stack, charIdx+1)
               '.' -> (Just Output, stack, charIdx+1)
               ',' -> (Just Input, stack, charIdx+1)
               '[' -> (Just JumpMarker, charIdx :: stack, charIdx+1)
               ']' -> (Just <| Jump <| unwrap "Unmatched brackets" <| List.head stack, List.drop 1 stack, charIdx+1)
               _   -> (Nothing, stack, charIdx)
