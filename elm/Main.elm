import Html exposing (Html,div,text,button,textarea,td,table,tr)
import Html.Attributes exposing (placeholder,style)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import Json.Encode exposing (int, list, object, string)
import Dict
   
import Elmscrew.Machine as Machine exposing (Machine)
import Elmscrew.Interpreter as Interpreter exposing (Interpreter)
import Elmscrew.Arbor exposing (displayGraph)
import Elmscrew.Parser as Parser exposing (parse)
import Elmscrew.Instruction as Instruction exposing (Inst)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { interp : Maybe Interpreter, output : String, program : String}

init : (Model, Cmd Msg)
init = (Model Nothing "" "", Cmd.none)

type Msg = NewContent String | Run | Step | BuildGraph
         | Right | Left | Inc | Dec | Output | Input

           
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleNewExecution interp output =
            ({model | interp = Just interp,
                      output = model.output ++ (Maybe.withDefault "" <| Maybe.map String.fromChar output)}
            , Cmd.none)
        
        handleStep result =
            case result of
                Interpreter.Running newInterp output -> handleNewExecution newInterp output
                Interpreter.Complete newInterp -> ({model | interp = Just newInterp}, Cmd.none)

        maybeInitInterpreter =
            case model.interp of
                Just interp -> interp
                Nothing -> Interpreter.initWithStr model.program

        executeInstruction inst =
            let (newInterp, newOutput) = Interpreter.execute maybeInitInterpreter inst in
            handleNewExecution newInterp newOutput
    in

    case msg of
        NewContent program -> ({model | program = program }, Cmd.none)
        BuildGraph -> let parsedProg = (Array.toList (parse model.program)) in
            (model, displayGraph (list (generateProgramGraphNodes parsedProg 0),
                                  list (generateProgramGraphEdges parsedProg 0)))
        Run ->
            let (newInterp, newOutput) =
                    Interpreter.runToCompletion "" (Interpreter.initWithStr model.program)
            in
                ({model | output = newOutput, interp = Just newInterp}, Cmd.none)

        Step -> handleStep <| Interpreter.step maybeInitInterpreter
        Right -> executeInstruction Instruction.Right
        Left -> executeInstruction Instruction.Left
        Inc -> executeInstruction Instruction.Inc
        Dec -> executeInstruction Instruction.Dec
        Output -> executeInstruction Instruction.Output
        Input -> executeInstruction Instruction.Input
        
        

getLoopEdges : Inst -> Int -> List Json.Encode.Value
getLoopEdges inst n = case inst of
                        Instruction.Jump i -> [object [ ("from", int n), ("to", int i) ]]
                        _ -> []

generateProgramGraphNodes : List Inst -> Int -> List Json.Encode.Value
generateProgramGraphNodes prog n =
    case prog of
        (x::xs) -> object [ ("id", int n),
                            ("label", string <| String.fromChar <| Instruction.toChar x) ]
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

buildTape : Maybe Interpreter -> Html msg
buildTape interp =
    let
        getData tape n = case (Dict.get n tape) of
                   Just data -> data
                   Nothing -> 0

        buildDataList tape n =
            if n == 512 then []
            else (td [] [ text <| toString <| getData tape n ]) :: buildDataList tape (n+1)

        buildHeaderList = (List.map (toString>>text>>List.singleton>>(td [])) (List.range 0 511))
                          
        tableStyle = style [ ("overflow-x", "scroll"), ("width", "700px") ]
                     
    in
        case interp of
            Just interp ->
                table [] [ div [tableStyle] [tr [] buildHeaderList, tr [] (buildDataList interp.machine.tape 0) ]]
            Nothing -> div[][]

makeInterpreterButtons = div [] <|
                         List.map (\x -> button [ onClick <| Tuple.first x] [ text <| Tuple.second x])
                             [
                              (Right, ">"),
                              (Left, "<"),
                              (Inc, "+"),
                              (Dec, "-"),
                              (Output, "."),
                              (Input, ",")
                             ]

view : Model -> Html Msg                             
view model =
    div []
        [ textarea [ placeholder "Program", onInput NewContent] []
        , button [ onClick Run ] [ text "Run" ]
        , button [ onClick Step ] [ text "Step" ]            
        , button [ onClick BuildGraph ] [ text "Visualise" ]
        , makeInterpreterButtons 
        , text model.output
        , buildTape model.interp
        ]

    
