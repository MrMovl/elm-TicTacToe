module TicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set, insert, member, foldl)
import Task


type alias Model =
    { currentPlayer : Player
    , winner : Maybe Player
    , crosses : Coordinates
    , circles : Coordinates
    , usedCells : Coordinates
    }


type alias Coordinates =
    Set Coordinate


type Player
    = Cross
    | Circle


type alias Coordinate =
    ( Int, Int )


initialModel : Model
initialModel =
    { currentPlayer = Cross
    , winner = Nothing
    , crosses = Set.empty
    , circles = Set.empty
    , usedCells = Set.empty
    }


type Msg
    = CellClicked Coordinate
    | Restart


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClicked coordinate ->
            ( takeTurn coordinate model, Cmd.none )

        Restart ->
            ( initialModel, Cmd.none )


takeTurn : Coordinate -> Model -> Model
takeTurn coord model =
    if member coord model.usedCells then
        model
    else
        case model.currentPlayer of
            Cross ->
                updateCrosses coord model

            Circle ->
                updateCircles coord model


updateCrosses : Coordinate -> Model -> Model
updateCrosses coord model =
    { currentPlayer = nextPlayer model.currentPlayer
    , winner = didSomeoneWin model coord
    , crosses = insert coord model.crosses
    , circles = model.circles
    , usedCells = insert coord model.usedCells
    }


updateCircles : Coordinate -> Model -> Model
updateCircles coord model =
    { currentPlayer = nextPlayer model.currentPlayer
    , winner = didSomeoneWin model coord
    , crosses = model.crosses
    , circles = insert coord model.circles
    , usedCells = insert coord model.usedCells
    }


didSomeoneWin : Model -> Coordinate -> Maybe Player
didSomeoneWin model newMove =
    case model.currentPlayer of
        Cross ->
            checkCells (insert newMove model.crosses) model.currentPlayer

        Circle ->
            checkCells (insert newMove model.circles) model.currentPlayer


checkCells : Coordinates -> Player -> Maybe Player
checkCells coords currentPlayer =
    Nothing
        |> checkDiagonal coords currentPlayer
        |> checkDefaultWin (Set.map (\( x, y ) -> x) coords) currentPlayer
        |> checkDefaultWin (Set.map (\( x, y ) -> y) coords) currentPlayer


checkDefaultWin : Set Int -> Player -> Maybe Player -> Maybe Player
checkDefaultWin coords currentPlayer potentialWinner =
    --    let
    --        cumulatedCoordinates =
    --            foldl (\c result -> ) ( 0, 0, 0 ) coords
    --    in
    potentialWinner


checkDiagonal : Coordinates -> Player -> Maybe Player -> Maybe Player
checkDiagonal coords currentPlayer potentialWinner =
    case potentialWinner of
        Just a ->
            potentialWinner

        Nothing ->
            if (slash coords || backslash coords) then
                Just currentPlayer
            else
                potentialWinner


slash : Coordinates -> Bool
slash coords =
    member ( 0, 0 ) coords && member ( 1, 1 ) coords && member ( 2, 2 ) coords


backslash : Coordinates -> Bool
backslash coords =
    member ( 0, 2 ) coords && member ( 1, 1 ) coords && member ( 2, 0 ) coords


nextPlayer : Player -> Player
nextPlayer currentPlayer =
    case currentPlayer of
        Cross ->
            Circle

        Circle ->
            Cross


view : Model -> Html Msg
view model =
    case model.winner of
        Nothing ->
            printGameView model

        Just player ->
            printWinScreen player


winMessage : String
winMessage =
    "Holy crap, you won this incredible game of wits. Congratulations, you are a superstar!"


drawMessage : String
drawMessage =
    "Well... that was boring. You guys are equally dull..."


printWinScreen : Player -> Html Msg
printWinScreen winner =
    div
        []
        [ h1 [] [ text "There can only be one" ]
        , div []
            [ div [ class "gameArea" ]
                [ h3 [] [ text winMessage ]
                , button [ onClick Restart ] [ text "Restart?" ]
                ]
            ]
        ]


printGameView : Model -> Html Msg
printGameView model =
    div []
        [ h1 [] [ text "There can only be one" ]
        , div []
            [ div [ class "gameArea" ]
                [ h3 [] [ model.currentPlayer |> toString |> (++) "Current Player: " |> text ]
                , table []
                    [ tr []
                        [ printTableCell ( 0, 0 ) model
                        , printTableCell ( 1, 0 ) model
                        , printTableCell ( 2, 0 ) model
                        ]
                    , tr []
                        [ printTableCell ( 0, 1 ) model
                        , printTableCell ( 1, 1 ) model
                        , printTableCell ( 2, 1 ) model
                        ]
                    , tr []
                        [ printTableCell ( 0, 2 ) model
                        , printTableCell ( 1, 2 ) model
                        , printTableCell ( 2, 2 ) model
                        ]
                    ]
                ]
            ]
        ]


printTableCell : Coordinate -> Model -> Html Msg
printTableCell coord model =
    let
        usedCells =
            case model.currentPlayer of
                Cross ->
                    model.crosses

                Circle ->
                    model.circles
    in
        td
            [ onClick (CellClicked coord) ]
            [ div
                [ marked coord model.crosses Cross
                , marked coord model.circles Circle
                ]
                []
            ]


marked : Coordinate -> Coordinates -> Player -> Html.Attribute a
marked coord usedCells player =
    if member coord usedCells then
        pickClass player |> class
    else
        class "none"


pickClass : Player -> String
pickClass player =
    case player of
        Cross ->
            "cross"

        Circle ->
            "circle"
