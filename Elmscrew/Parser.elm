module Elmscrew.Parser exposing (parse, toChar, Inst(..), serializeProgram)

import Elmscrew.Utils exposing (unwrap)

import Array exposing (Array)

type Inst = Right | Left | Inc | Dec | Output | Input | Loop Int

toChar : Inst -> Char
toChar inst = case inst of
                  Right -> '>' 
                  Left -> '<' 
                  Inc -> '+' 
                  Dec -> '-' 
                  Output -> '.' 
                  Input -> ',' 
                  Loop i -> ']'
                  
                  
    
toInst : Int -> List Int -> Char -> (Maybe Inst, List Int)
toInst charIdx stack c = case c of
               '>' -> (Just Right, stack)
               '<' -> (Just Left, stack)
               '+' -> (Just Inc, stack)
               '-' -> (Just Dec, stack)
               '.' -> (Just Output, stack)
               ',' -> (Just Input, stack)
               '[' -> (Nothing, charIdx :: stack)
               ']' -> (Just <| Loop <| unwrap "Unmatched brackets" <| List.head stack, List.drop 1 stack)
               _   -> (Nothing, stack)

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
            let (inst, newStack) = (toInst charIdx stack x) in
                inst :: buildProg (charIdx+1) newStack xs
        [] ->
            []


