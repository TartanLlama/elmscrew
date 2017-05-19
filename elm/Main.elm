import Html exposing (Html,div,text,button,textarea,td,table,tr,h1,h3,h5,a)
import Html.Attributes exposing (placeholder,style,href)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import Json.Encode exposing (int, list, object, string)
import Platform.Cmd exposing (Cmd)
import Dict

import Elmscrew.Machine as Machine exposing (Machine)
import Elmscrew.Interpreter as Interpreter exposing (Interpreter)
import Elmscrew.Vis exposing (displayGraph,sendCurrentNode)
import Elmscrew.Parser as Parser exposing (parse)
import Elmscrew.Instruction as Instruction exposing (Inst)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { interp : Maybe Interpreter
                   , output : String
                   , input : String
                   , program : String}

init : (Model, Cmd Msg)
init = (Model Nothing "" "" "", Cmd.none)

-- Reset the model, but keep the current program
reset : Model -> (Model, Cmd Msg)
reset model = (Model Nothing "" "" model.program, Cmd.none)

type Msg = NewProgram String | NewInput String | Run | Step | Reset
         | Right | Left | Inc | Dec | Output | Input

-- Sends a message to javascript with the node which is currently being executed
setCurrentNode : Interpreter -> Cmd Msg
setCurrentNode interp =
    let id = if interp.pc == (Array.length interp.instructions)
             then "end"
             else toString interp.pc
    in
        sendCurrentNode id

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        -- Updates the model and current node after a step or manual instruction execution
        handleNewExecution interp input output =
            ({model | interp = Just interp,
                      input = input,
                      output = model.output ++ (Maybe.withDefault "" <| Maybe.map String.fromChar output)}
            , setCurrentNode interp)

        -- Updates the model and current node after a step
        handleStep result =
            case result of
                Interpreter.Running newInterp newInput output -> handleNewExecution newInterp newInput output
                Interpreter.Complete newInterp -> ({model | interp = Just newInterp},
                                                   setCurrentNode newInterp)

        -- Gets the current interpreter, initializing one if there isn't one set up yet
        maybeInitInterpreter =
            case model.interp of
                Just interp -> interp
                Nothing -> Interpreter.initWithStr model.program

        -- Executes a given instruction and updates the model
        executeInstruction inst =
            let (newInterp, newInput, newOutput) = Interpreter.execute maybeInitInterpreter inst model.input in
            handleNewExecution newInterp newInput newOutput
    in

    case msg of
        -- Update the program in the model and build a graph to visualise
        NewProgram program ->
            let parsedProg = (Array.toList (parse program)) in
            ({model | program = program }, displayGraph (list (generateProgramGraphNodes parsedProg 0),
                                                         list (generateProgramGraphEdges parsedProg 0)))

        -- Update the input in the model
        NewInput input -> ({model | input = input }, Cmd.none)

        -- Reset the model
        Reset -> reset model

        -- Run the program to completion
        Run ->
            let (newInterp, newOutput) =
                    Interpreter.runToCompletion "" (Interpreter.initWithStr model.program) model.input
            in
                ({model | output = newOutput, interp = Just newInterp}, setCurrentNode newInterp)

        -- This group of commands allows users to submit their own instructions to the interpreter
        Step -> handleStep <| Interpreter.step maybeInitInterpreter model.input
        Right -> executeInstruction Instruction.Right
        Left -> executeInstruction Instruction.Left
        Inc -> executeInstruction Instruction.Inc
        Dec -> executeInstruction Instruction.Dec
        Output -> executeInstruction Instruction.Output
        Input -> executeInstruction Instruction.Input

-- Gives back any additional loop edges needed for an instruction
getLoopEdges : Inst -> Int -> List Json.Encode.Value
getLoopEdges inst n = case inst of
                        Instruction.Jump i -> [object [ ("from", int n), ("to", int i) ]]
                        _ -> []

-- Create graph nodes for every instruction in a program
generateProgramGraphNodes : List Inst -> Int -> List Json.Encode.Value
generateProgramGraphNodes prog n =
    case prog of
        (x::xs) -> object [ ("id", int n),
                            ("label", string <| String.fromChar <| Instruction.toChar x) ]
                   :: generateProgramGraphNodes xs (n+1)
        _ -> List.map (\name -> object [("id", string name),("label", string name)]) ["start", "end"]

-- Create edges between sibling instructions and matched loops
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

-- Build HTML to represent the tape memory
buildTape : Maybe Interpreter -> Html msg
buildTape interp =
    let
        -- Get the current data for the given memory location, returning 0 if there is nothing there
        getData tape n = Maybe.withDefault 0 (Dict.get n tape)

        -- Colour for a highlighted node
        highlightedNodeStyle = style [ ("background", "#9b4dca"), ("color", "#fff") ]

        -- Highlights a node if it is the current memory location in use
        nodeStyle cursor n = if n == cursor then [highlightedNodeStyle] else []

        -- Builds a table row containing the contents of memory
        buildDataList tape cursor n =
            if n == 512 then []
            else (td (nodeStyle cursor n) [ (getData tape n)|>toString|>text ]) :: (buildDataList tape cursor (n+1))

        buildTdNode cursor n = td (nodeStyle cursor n) [n |> toString |> text]
        buildHeaderList cursor = (List.map (buildTdNode cursor) (List.range 0 511))

        tableStyle = style [ ("overflow-x", "scroll"), ("width", "700px") ]

        -- Build a table full of zeros
        buildBlankTable = [tr [] (List.map (toString>>text>>List.singleton>>(td [])) (List.range 0 511))
                          ,tr [] (List.repeat 512 (td [] [text "0"]))]

    in
        case interp of
            Just interp ->
                table [] [ div [tableStyle] [tr [] (buildHeaderList interp.machine.position)
                                            ,tr [] (buildDataList interp.machine.tape interp.machine.position 0) ]
                         ]
            Nothing -> table [] [ div [tableStyle] buildBlankTable ]

-- Make buttons for all of the interpreter actions
makeInterpreterButtons = div [] <|
                         List.map (\x -> button [ onClick <| Tuple.first x] [ text <| Tuple.second x])
                             [
                              (Left, "<"),
                              (Right, ">"),
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
        , textarea [ placeholder "Input", inputOutputStyle "left", onInput NewInput ] [ text model.input ]
        , textarea [ placeholder "Output", inputOutputStyle "right"] [ text model.output ]
        , div [] [ button [ onClick Run ] [ text "Run" ]
                 , button [ onClick Step ] [ text "Step" ]
                 , button [ onClick Reset ] [ text "Reset" ]
                 ]
        , makeInterpreterButtons
        , buildTape model.interp
        ]
