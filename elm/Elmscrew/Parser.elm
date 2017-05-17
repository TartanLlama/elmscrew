module Elmscrew.Parser exposing (parse, toChar, Inst(..))

import Elmscrew.Utils exposing (unwrap)

import Array exposing (Array)

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

isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ -> True
        Nothing -> False

parse : String -> Array Inst
parse str =
    str
        |> String.toList
        |> buildProg 0 []
        |> List.filter isJust
        |> List.map (Maybe.withDefault Input)
        |> Array.fromList


buildProg : Int -> List Int -> List Char -> List (Maybe Inst)
buildProg charIdx stack chars =
    case chars of
        (x::xs) ->
            let (inst, newStack, newCharIdx) = (toInst charIdx stack x) in
                inst :: buildProg newCharIdx newStack xs
        [] ->
            []


