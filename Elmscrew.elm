import Html exposing (Html,div,text,button,input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import Json.Encode exposing (int, list, object, string)

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
        BuildGraph -> let parsedProg = (Array.toList (parse model.program)) in
            (model, displayGraph (list (generateProgramGraphNodes parsedProg 0),
                                  list (generateProgramGraphEdges parsedProg 0)))
        run -> ({model |
                     output = Tuple.second <|
                              Interpreter.runToCompletion 0 "" (Interpreter.initWithStr (\x->'\0') model.program)}, Cmd.none)

getLoopEdges : Inst -> Int -> List Json.Encode.Value
getLoopEdges inst n = case inst of
                        Jump i -> [object [ ("from", int n), ("to", int i) ]]
                        _ -> []

generateProgramGraphNodes : List Inst -> Int -> List Json.Encode.Value
generateProgramGraphNodes prog n =
    case prog of
        (x::xs) -> object [ ("id", int n),
                            ("label", string <| String.fromChar <| Parser.toChar x) ]
                   :: generateProgramGraphNodes xs (n+1)
        _ -> []

generateProgramGraphEdges : List Inst -> Int -> List Json.Encode.Value
generateProgramGraphEdges prog n =
    case prog of    
        (x::y::xs) -> [object [ ("from", int n), ("to", int (n+1)) ]]
                      ++ (getLoopEdges x n)
                      ++ generateProgramGraphEdges (y::xs) (n+1)
                          
        (x::y) -> (getLoopEdges x n) ++ (generateProgramGraphEdges (y) (n+1))
        _ -> []
    
subscriptions : model -> Sub msg
subscriptions model = Sub.none

view model =
    div []
        [ input [ placeholder "Program", onInput NewContent] []
        , button [ onClick Run ] [ text "Run" ]
        , button [ onClick BuildGraph ] [ text "Visualise" ]            
        , text model.output
        ]

    
