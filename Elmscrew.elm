import Html exposing (Html,div,text,button,input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)

import Elmscrew.Machine as Machine exposing (Machine)
import Elmscrew.Interpreter as Interpreter exposing (Interpreter)
import Elmscrew.Arbor exposing (displayGraph)
import Elmscrew.Parser as Parser exposing (Inst(..), parse)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { machine : Machine, output : String, program : String}

init : (Model, Cmd Msg)
init = (Model Machine.init "" "", Cmd.none)

type Msg = NewContent String | Run | BuildGraph

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewContent program -> ({model | program = program }, Cmd.none)
        BuildGraph -> (model, displayGraph <| serializeProgram (Array.toList (parse model.program)) 0)
        Run -> ({model |
                     output = Tuple.second <|
                              Interpreter.runToCompletion 0 "" (Interpreter.initWithStr (\x->'\0') model.program)}, Cmd.none)

serializeProgram : List Inst -> Int -> List (String, List Int)
serializeProgram prog n = case prog of
                            (x::y::ys) -> (String.fromChar (Parser.toChar x), [n+1]) :: serializeProgram (y::ys) (n+1)
                            (x::y) -> (String.fromChar (Parser.toChar x), []) :: serializeProgram (y) (n+1)
                            _ -> []
                                                  
subscriptions : model -> Sub msg
subscriptions model = Sub.none

view model =
    div []
        [ input [ placeholder "Program", onInput NewContent] []
        , button [ onClick BuildGraph ] []
        , text model.output
        ]

    
