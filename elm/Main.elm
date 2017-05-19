import Html exposing (Html,div,text,button,textarea,td,table,tr,h1,h3,h5,a)
import Html.Attributes exposing (placeholder,style,href)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import Json.Encode exposing (int, list, object, string)
import Platform.Cmd exposing (Cmd)
import Dict

import Elmscrew.Machine as Machine exposing (Machine)
import Elmscrew.Interpreter as Interpreter exposing (Interpreter)
import Elmscrew.Arbor exposing (displayGraph,setCurrentNode)
import Elmscrew.Parser as Parser exposing (parse)
import Elmscrew.Instruction as Instruction exposing (Inst)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { interp : Maybe Interpreter, output : String, input : String, program : String}

init : (Model, Cmd Msg)
init = (Model Nothing "" "" "", Cmd.none)

type Msg = NewProgram String | NewInput String | Run | Step | BuildGraph
         | Right | Left | Inc | Dec | Output | Input


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleNewExecution interp output =
            ({model | interp = Just interp,
                      output = model.output ++ (Maybe.withDefault "" <| Maybe.map String.fromChar output)}
            , setCurrentNode interp.pc)

        handleStep result =
            case result of
                Interpreter.Running newInterp output -> handleNewExecution newInterp output
                Interpreter.Complete newInterp -> ({model | interp = Just newInterp},
                                                   setCurrentNode newInterp.pc)

        maybeInitInterpreter =
            case model.interp of
                Just interp -> interp
                Nothing -> Interpreter.initWithStr model.program

        executeInstruction inst =
            let (newInterp, newOutput) = Interpreter.execute maybeInitInterpreter inst in
            handleNewExecution newInterp newOutput
    in

    case msg of
        NewProgram program -> ({model | program = program }, Cmd.none)
        NewInput input -> ({model | input = input }, Cmd.none)
        BuildGraph -> let parsedProg = (Array.toList (parse model.program)) in
            (model, displayGraph (list (generateProgramGraphNodes parsedProg 0),
                                  list (generateProgramGraphEdges parsedProg 0)))
        Run ->
            let (newInterp, newOutput) =
                    Interpreter.runToCompletion "" (Interpreter.initWithStr model.program)
            in
                ({model | output = newOutput, interp = Just newInterp}, setCurrentNode newInterp.pc)

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
        _ -> List.map (\name -> object [("id", string name),("label", string name)]) ["start", "end"]

generateProgramGraphEdges : List Inst -> Int -> List Json.Encode.Value
generateProgramGraphEdges prog n =
    case prog of
        (x::y::xs) -> [object [ ("from", int n), ("to", int (n+1)) ]]
                      ++ (getLoopEdges x n)
                      ++ generateProgramGraphEdges (y::xs) (n+1)

        (x::y) -> (getLoopEdges x n) ++ (generateProgramGraphEdges (y) (n+1))
        _ -> [object [("from", string "start"),("to", int 0)],
              object [("from", int (n-1)),("to", string "end")]]

subscriptions : model -> Sub msg
subscriptions model = Sub.none

buildTape : Maybe Interpreter -> Html msg
buildTape interp =
    let
        getData tape n = case (Dict.get n tape) of
                   Just data -> data
                   Nothing -> 0

        highlightedNodeStyle = style [ ("background", "#9b4dca"), ("color", "#fff") ]
        nodeStyle cursor n = if n == cursor then [highlightedNodeStyle] else []

        buildDataList tape cursor n =
            if n == 512 then []
            else (td (nodeStyle cursor n) [ (getData tape n)|>toString|>text ]) :: (buildDataList tape cursor (n+1))

        buildTdNode cursor n = td (nodeStyle cursor n) [n |> toString |> text]
        buildHeaderList cursor = (List.map (buildTdNode cursor) (List.range 0 511))

        tableStyle = style [ ("overflow-x", "scroll"), ("width", "700px") ]

        buildBlankTable = [tr [] (List.map (toString>>text>>List.singleton>>(td [])) (List.range 0 511))
                          ,tr [] (List.repeat 512 (td [] [text "0"]))]

    in
        case interp of
            Just interp ->
                table [] [ div [tableStyle] [tr [] (buildHeaderList interp.machine.position)
                                            ,tr [] (buildDataList interp.machine.tape interp.machine.position 0) ]
                         ]
            Nothing -> table [] [ div [tableStyle] buildBlankTable ]

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
    let
        inputOutputStyle float = style [("width", "140px"), ("height", "88px"), ("float", float)]
    in

    div [style [("width", "700px"), ("margin", "auto"), ("text-align", "center")]]
        [ h1 [] [ text "Elmscrew" ]
        , h3 [] [ text "A "
                , a [href "https://en.wikipedia.org/wiki/Brainfuck"] [text "Brainfuck"]
                , text " interpreter and debugger written in "
                , a [href "http://elm-lang.org/"] [text "Elm"]
                ]
        , h5 [] [ text "By "
                , a [href "https://blog.tartanllama.xyz/"] [text "TartanLlama"]
                ]
        , textarea [ placeholder "Program", onInput NewProgram] []
        , textarea [ placeholder "Input", inputOutputStyle "left", onInput NewInput ] []
        , textarea [ placeholder "Output", inputOutputStyle "right"] [ text model.output ]
        , div [] [ button [ onClick Run ] [ text "Run" ]
                 , button [ onClick Step ] [ text "Step" ]
                 , button [ onClick BuildGraph ] [ text "Visualise" ]
                 ]
        , makeInterpreterButtons
        , buildTape model.interp
        ]
