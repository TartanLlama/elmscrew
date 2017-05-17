module Elmscrew.Utils exposing (unwrap)

unwrap msg maybe =
    case maybe of
        Just a -> a
        Nothing -> Debug.crash msg
