module Elmscrew.Parser exposing (parse)

import Elmscrew.Instruction exposing (Inst(..), toInst)

import Array exposing (Array)

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


