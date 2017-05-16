import Html exposing (Html,div,text,button,input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Elmscrew.Machine as Machine exposing (Machine)
import Elmscrew.Interpreter as Interpreter exposing (Interpreter)

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

type Msg = NewContent String | Run 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewContent program -> ({model | program = program }, Cmd.none)
        Run -> ({model |
                     output = Tuple.second <|
                              Interpreter.runToCompletion 0 "" (Interpreter.init (\x->'\0') model.program)}, Cmd.none)

subscriptions : model -> Sub msg
subscriptions model = Sub.none
               
view model =
    div []
        [ input [ placeholder "Program", onInput NewContent] []
        , button [ onClick Run ] []
        , text model.output
        ]

    
