module TicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set, insert, member)
import Array exposing (Array)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



------------ Model ------------


type alias Model =
    { currentPlayer : Player
    , winner : Maybe Player
    , draw : Bool
    , crosses : Coordinates
    , circles : Coordinates
    , usedCells : Coordinates
    }


type alias Coordinates =
    Set Coordinate


type alias Coordinate =
    ( Int, Int )


type Player
    = Cross
    | Circle


initialModel : Model
initialModel =
    { currentPlayer = Cross
    , winner = Nothing
    , draw = False
    , crosses = Set.empty
    , circles = Set.empty
    , usedCells = Set.empty
    }



------------ View ------------


view : Model -> Html Msg
view model =
    case model.winner of
        Nothing ->
            if model.draw then
                printDrawView
            else
                printGameView model

        Just player ->
            printWinScreen player


printDrawView : Html Msg
printDrawView =
    div
        []
        [ h1 [] [ text "Draw game!" ]
        , div []
            [ div [ class "gameArea" ]
                [ h3 [] [ text drawMessage ]
                , button [ onClick Restart ] [ text "Restart?" ]
                ]
            ]
        ]


printWinScreen : Player -> Html Msg
printWinScreen winner =
    div
        []
        [ h1 [] [ text (stringifyPlayer winner ++ " won!") ]
        , div []
            [ div [ class "gameArea" ]
                [ h3 [] [ text winMessage ]
                , button [ onClick Restart ] [ text "Restart?" ]
                ]
            ]
        ]


winMessage : String
winMessage =
    "Holy crap, you won this incredible game of wits. Congratulations, you are a superstar!"


drawMessage : String
drawMessage =
    "Well... that was boring. You guys are equally dull..."


printGameView : Model -> Html Msg
printGameView model =
    div []
        [ title
        , div []
            [ div [ class "gameArea" ]
                [ h3 [] [ currentPlayerText model.currentPlayer ]
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


title : Html Msg
title =
    h1 [] [ text "There can only be one" ]


currentPlayerText : Player -> Html Msg
currentPlayerText currentPlayer =
    currentPlayer |> stringifyPlayer |> (++) "Current Player: " |> text


printTableCell : Coordinate -> Model -> Html Msg
printTableCell coord model =
    td
        [ onClick (CellClicked coord) ]
        [ div
            [ markCell coord model.crosses Cross
            , markCell coord model.circles Circle
            ]
            []
        ]


markCell : Coordinate -> Coordinates -> Player -> Html.Attribute a
markCell coord usedCells player =
    if Set.member coord usedCells then
        stringifyPlayer player |> class
    else
        class "none"


stringifyPlayer : Player -> String
stringifyPlayer player =
    case player of
        Cross ->
            "cross"

        Circle ->
            "circle"



------------ Update ------------


type Msg
    = CellClicked Coordinate
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( initialModel, Cmd.none )

        CellClicked coordinate ->
            ( takeTurn coordinate model, Cmd.none )


takeTurn : Coordinate -> Model -> Model
takeTurn coord model =
    if Set.member coord model.usedCells then
        model
    else
        case model.currentPlayer of
            Cross ->
                updateCrosses coord model

            Circle ->
                updateCircles coord model


updateCrosses : Coordinate -> Model -> Model
updateCrosses coord model =
    let
        maybeWinner =
            didSomeoneWin model coord

        newCells =
            Set.insert coord model.usedCells
    in
        { currentPlayer = nextPlayer model.currentPlayer
        , winner = maybeWinner
        , draw = maybeWinner == Nothing && allCellsTaken newCells
        , crosses = Set.insert coord model.crosses
        , circles = model.circles
        , usedCells = newCells
        }


updateCircles : Coordinate -> Model -> Model
updateCircles coord model =
    let
        maybeWinner =
            didSomeoneWin model coord

        newCells =
            Set.insert coord model.usedCells
    in
        { currentPlayer = nextPlayer model.currentPlayer
        , winner = maybeWinner
        , draw = maybeWinner == Nothing && allCellsTaken newCells
        , crosses = model.crosses
        , circles = Set.insert coord model.circles
        , usedCells = newCells
        }


nextPlayer : Player -> Player
nextPlayer currentPlayer =
    case currentPlayer of
        Cross ->
            Circle

        Circle ->
            Cross


allCellsTaken : Coordinates -> Bool
allCellsTaken coords =
    Set.size coords == 9


didSomeoneWin : Model -> Coordinate -> Maybe Player
didSomeoneWin model newMove =
    let
        cellsToCheck =
            case model.currentPlayer of
                Cross ->
                    Set.insert newMove model.crosses

                Circle ->
                    Set.insert newMove model.circles
    in
        checkCells cellsToCheck model.currentPlayer


checkCells : Coordinates -> Player -> Maybe Player
checkCells coords currentPlayer =
    let
        coordsAsList =
            coords |> Set.toList |> Array.fromList

        xCoords =
            Array.map (\( x, y ) -> x) coordsAsList

        yCoords =
            Array.map (\( x, y ) -> y) coordsAsList
    in
        checkDiagonal coords currentPlayer
            |> checkDefaultWin xCoords currentPlayer
            |> checkDefaultWin yCoords currentPlayer


checkDiagonal : Coordinates -> Player -> Maybe Player
checkDiagonal coords currentPlayer =
    if (slash coords || backslash coords) then
        Just currentPlayer
    else
        Nothing


slash : Coordinates -> Bool
slash coords =
    member ( 0, 0 ) coords
        && member ( 1, 1 ) coords
        && member ( 2, 2 ) coords


backslash : Coordinates -> Bool
backslash coords =
    member ( 0, 2 ) coords
        && member ( 1, 1 ) coords
        && member ( 2, 0 ) coords



{-
   This is complicated...
   This function takes all x values or all y values and counts how often
   a row or column is used.
   For example, if we check the xCoords, we take all x-es and count the
   occurence of zeros, ones and twos. If either of those appears three
   times, we have a full column.
-}


checkDefaultWin : Array Int -> Player -> Maybe Player -> Maybe Player
checkDefaultWin coords currentPlayer potentialWinner =
    let
        initialBuckets =
            Array.fromList [ 0, 0, 0 ]

        cumulatedCoordinates =
            Array.foldl incrementAtPosition initialBuckets coords

        threeInARow =
            cumulatedCoordinates |> Array.filter (\e -> e == 3)
    in
        case potentialWinner of
            Just a ->
                potentialWinner

            Nothing ->
                if Array.length threeInARow > 0 then
                    Just currentPlayer
                else
                    Nothing


incrementAtPosition : Int -> Array Int -> Array Int
incrementAtPosition pos array =
    let
        new =
            Array.get pos array |> Maybe.withDefault 0 |> (+) 1
    in
        Array.set pos new array
