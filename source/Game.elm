module Game exposing (..)

import Array exposing (..)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Task exposing (..)
import Time exposing (..)
import Url exposing (Url)



-- TODO: a piece can't move twice in one player round
-- TODO: infantry can't move from an engaged position to another engaged position
-- TODO: implement infantry capture
-- TODO: implement artillery capture
-- TODO: reinforcement


type alias Model =
    { board : Board
    , turn : Turn
    , coordinatesBombardedByBlue : Set CoordinateTuple
    , coordinatesBombardedByRed : Set CoordinateTuple
    , potentialMoves : Set CoordinateTuple
    , selected : Selection
    , computerThinking : Bool
    , gameOver : Bool
    , redReinforcements : List Piece
    , blueReinforcements : List Piece
    }


type Msg
    = PositionClicked Position
    | StartEngine
    | ComputerMove (Maybe Move)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


type alias CoordinateTuple =
    ( Int, Int )


type Selection
    = NoSelection
    | SelectedPosition Position


type alias Flags =
    { gameState : String
    }


startingBoard : Board
startingBoard =
    let
        x =
            [ [ Occupied Red Headquarters, Occupied Red (Artillery Artillery1 S), Vacant, Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Occupied Red (Infantry Infantry1), Occupied Red (Infantry Infantry2), Occupied Red (Infantry Infantry3), Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Occupied Blue (Infantry Infantry3), Occupied Blue (Infantry Infantry2), Occupied Blue (Infantry Infantry1) ]
            , [ Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Occupied Blue (Artillery Artillery1 N), Occupied Blue Headquarters ]
            ]
    in
    Array.fromList (List.map Array.fromList x)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( updateBombardmentPositions
        { board = startingBoard
        , turn = BluesTurn First
        , potentialMoves = Set.empty
        , coordinatesBombardedByBlue = Set.empty
        , coordinatesBombardedByRed = Set.empty
        , selected = NoSelection
        , computerThinking = False
        , gameOver = False
        , redReinforcements = initialReinforcements S
        , blueReinforcements = initialReinforcements N
        }
    , Cmd.none
    )


initialReinforcements : Facing -> List Piece
initialReinforcements facing =
    [ Infantry Infantry4
    , Infantry Infantry5
    , Infantry Infantry6
    , Infantry Infantry7
    , ArmoredInfantry ArmoredInfantry1
    , ArmoredInfantry ArmoredInfantry2
    , ArmoredInfantry ArmoredInfantry3
    , Paratrooper
    , Artillery Artillery1 facing
    , Artillery Artillery2 facing
    , ArmoredArtillery facing
    , HeavyArtillery facing
    ]


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.computerThinking && not (bluesTurn model.turn) then
        Time.every 50 startEngine

    else
        Sub.none


updateBombardmentPositions : Model -> Model
updateBombardmentPositions model =
    { model
        | coordinatesBombardedByBlue = calculateUnderBombardmentByPlayer Blue model.board
        , coordinatesBombardedByRed = calculateUnderBombardmentByPlayer Red model.board
    }


startEngine : Posix -> Msg
startEngine time =
    StartEngine


executeEngine : Model -> Cmd Msg
executeEngine model =
    Task.perform ComputerMove (nextMove model)


updatePositionClicked : Position -> Model -> ( Model, Cmd Msg )
updatePositionClicked position model =
    let
        nextModel =
            processMouseUp position model
    in
    ( updateBombardmentPositions nextModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PositionClicked position ->
            updatePositionClicked position model

        StartEngine ->
            ( { model | computerThinking = True }, executeEngine model )

        ComputerMove maybeMove ->
            case maybeMove of
                Just ( srcPosition, destPosition ) ->
                    ( processMove srcPosition destPosition model, Cmd.none )

                Nothing ->
                    ( { model | gameOver = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


clearSelected : Model -> Model
clearSelected model =
    { model | selected = NoSelection }


setSelected : Selection -> Model -> Model
setSelected selection model =
    { model | selected = selection }


clearMoves : Model -> Model
clearMoves model =
    { model | potentialMoves = Set.empty }


squareSize =
    64



--
--fromFloatPair : ( Float, Float ) -> Position
--fromFloatPair ( x, y ) =
--    Position OnBoard (floor (x / squareSize)) (floor (y / squareSize))


determineMoves : Position -> Model -> Set ( Int, Int )
determineMoves position model =
    validMoves position model |> List.filterMap positionToMaybeCoordinate |> Set.fromList


positionToMaybeCoordinate : Position -> Maybe ( Int, Int )
positionToMaybeCoordinate position =
    case position of
        OnBoard coordinate ->
            Just ( coordinate.x, coordinate.y )

        _ ->
            Nothing


updatePotentialMoves : Model -> Model
updatePotentialMoves model =
    case model.selected of
        SelectedPosition position ->
            { model | potentialMoves = determineMoves position model }

        _ ->
            model


processMove : Position -> Position -> Model -> Model
processMove src dest model =
    let
        updatedGameModel =
            updateGame ( src, dest ) model
    in
    updateBombardmentPositions
        ({ updatedGameModel | computerThinking = False }
            |> clearSelected
            |> clearMoves
        )


processMouseUp : Position -> Model -> Model
processMouseUp position model =
    let
        gameSquare =
            getGameSquare position model.board
    in
    case model.selected of
        SelectedPosition selectedPosition ->
            if position == selectedPosition then
                clearSelected model |> clearMoves

            else
                case gameSquare of
                    Just squareValue ->
                        case squareValue of
                            Vacant ->
                                model

                            Occupied player piece ->
                                if player == Blue then
                                    model |> setSelected (SelectedPosition position) |> updatePotentialMoves

                                else
                                    model

                    Nothing ->
                        model

        NoSelection ->
            case gameSquare of
                Just squareValue ->
                    case squareValue of
                        Vacant ->
                            model

                        Occupied player piece ->
                            if (player == Blue) && bluesTurn model.turn then
                                model |> setSelected position |> updatePotentialMoves

                            else if (player == Red) && not (bluesTurn model.turn) then
                                model |> setSelected position |> updatePotentialMoves

                            else
                                model

                Nothing ->
                    model


bluesTurn : Turn -> Bool
bluesTurn turn =
    case turn of
        BluesTurn _ ->
            True

        _ ->
            False


px : Int -> String
px value =
    String.fromInt value ++ "px"


tile : Model -> Int -> Int -> Html Msg
tile model xPos yPos =
    let
        xStr =
            (xPos * squareSize) |> px

        yStr =
            (yPos * squareSize) |> px

        position =
            OnBoard { x = xPos, y = yPos }

        background =
            if underBombardmentByPlayer Red position model then
                "#FF5555"

            else if underBombardmentByPlayer Blue position model then
                "#5555FF"

            else
                "white"

        borderStyle =
            if model.selected == SelectedPosition position then
                "4px solid #0000FF"

            else
                "1px solid black"
    in
    span
        [ onClick (PositionClicked position)
        , style "background" background
        , style "border" borderStyle
        , style "width" (px squareSize)
        , style "height" (px squareSize)
        , style "text-align" "center"
        , style "position" "absolute"
        , style "left" xStr
        , style "top" yStr
        ]
        []



--
--imageName: Player -> Piece -> String
--imageName player piece =
--    case piece of
--        Infantry ->
--            "infantry"
--
--        ArmoredInfantry ->
--            "armored_infantry"
--
--        Paratrooper ->
--            "paratrooper"
--
--        Artillery facing ->
--
--
--        HeavyArtillery facing ->
--
--
--        ArmoredArtillery facing ->
--
--
--        Headquarters ->
--
--
--chooseImage: Player -> Piece -> String
--chooseImage player piece =
--    "images/" ++ toColor player ++ (imageName player piece ) ++ ".png"
--


pieceToCharacter : Piece -> String
pieceToCharacter piece =
    case piece of
        Infantry _ ->
            "->"

        ArmoredInfantry _ ->
            "=>"

        Paratrooper ->
            "-)"

        Artillery _ _ ->
            "**"

        HeavyArtillery _ ->
            "***"

        ArmoredArtillery _ ->
            "oo**"

        Headquarters ->
            "X"


renderPiece : Model -> Player -> Piece -> Position -> Html Msg
renderPiece model player piece position =
    let
        borderStyle =
            if model.selected == SelectedPosition position then
                "4px solid #5495B6"

            else
                case position of
                    OnBoard c ->
                        if Set.member ( c.x, c.y ) model.potentialMoves then
                            "4px dotted red"

                        else
                            ""

                    _ ->
                        ""
    in
    -- TODO: implement images
    --Html.img
    div
        [ onClick (PositionClicked position)

        --, Html.Attributes.src (chooseImage player piece)
        , style "cursor" "grab"
        , style "width" (px squareSize)
        , style "height" (px squareSize)
        , style "position" "absolute"
        , style "color"
            (if player == Blue then
                "blue"

             else
                "red"
            )
        , style "border" borderStyle
        ]
        [ text (pieceToCharacter piece)
        ]


type alias File =
    List ( Position, GameSquare )


type alias EvaluatorFunc =
    Player -> Model -> Moves -> Float



-- An extensible evaluation model


evaluators : List EvaluatorFunc
evaluators =
    [ pieceTotalPrice

    --, ( mobility, 0.1 )
    , space

    --, ( isolatedArtillery, -0.5 )
    , center
    , threats
    ]


eval : Model -> Float
eval model =
    performEval model


execute : Player -> Player -> Model -> Moves -> Moves -> EvaluatorFunc -> Float
execute player1 player2 model player1Moves player2Moves evaluatorFunc =
    evaluatorFunc player1 model player1Moves - evaluatorFunc player2 model player2Moves


performEval : Model -> Float
performEval model =
    let
        player1 =
            Red

        player2 =
            Blue

        availableMoves =
            allAvailableMoves player1 model

        opponentMoves =
            allAvailableMoves player2 model
    in
    List.map (execute player1 player2 model availableMoves opponentMoves) evaluators
        |> List.foldr (+) 0.0


centerPredicate : Player -> ( Position, GameSquare ) -> Bool
centerPredicate player tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    case gameSquare of
        Occupied p _ ->
            (position.x == 3 || position.x == 4) && (position.y == 3 || position.y == 4)

        _ ->
            False


center : Player -> Model -> Moves -> Float
center player model moves =
    let
        playerInCenter =
            findPiecesBy (centerPredicate player) model.board |> List.length

        v =
            toFloat playerInCenter

        weight =
            0.5
    in
    v * weight


occupiedBy : Player -> Board -> Position -> Maybe Position
occupiedBy player board position =
    let
        maybeGameSquare =
            getGameSquare position board
    in
    case maybeGameSquare of
        Just value ->
            case value of
                Occupied p _ ->
                    if p == player then
                        Just position

                    else
                        Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


threats : Player -> Model -> Moves -> Float
threats player model moves =
    let
        v =
            List.map (\( src, dest ) -> dest) moves
                |> List.filterMap (occupiedBy (opponent player) model.board)
                |> List.length
                |> toFloat

        weight =
            0.5
    in
    v * weight


yComparison a b =
    let
        ( posA, sqA ) =
            a

        ( posB, sqB ) =
            b
    in
    compare posA.y posB.y



--file : Model -> Int -> File
--file model index =
--    findPiecesBy (\( position, gameSquare ) -> position.x == index) model.board
--        |> List.sortWith yComparison
--
--
--adjacentFiles : Model -> Int -> ( File, File )
--adjacentFiles model fileIndex =
--    ( file model (fileIndex - 1), file model (fileIndex + 1) )
--
--
--toSetHelper : List ( Position, GameSquare ) -> Set Int -> Set Int
--toSetHelper list set =
--    let
--        hd =
--            List.head list
--    in
--    case hd of
--        Just tuple ->
--            Set.insert (Tuple.first tuple).x set
--
--        Nothing ->
--            set
--toSet : List ( Position, GameSquare ) -> Set Int
--toSet list =
--    toSetHelper list Set.empty
--
--
--filterForPawns player piece ( position, gameSquare ) =
--    case gameSquare of
--        Occupied _ _ ->
--            True
--
--        _ ->
--            False
--countInFile : File -> Piece -> Player -> Int
--countInFile theFile piece player =
--    List.filter (filterForPawns player piece) theFile
--        |> List.length
--
--
--countIsolated : Player -> ( File, File ) -> Int -> Int
--countIsolated player ( file1, file2 ) accumulator =
--    let
--        count1 =
--            countInFile file1 Pawn player
--
--        count2 =
--            countInFile file2 Pawn player
--    in
--    if count1 + count2 == 0 then
--        accumulator + 1
--
--    else
--        accumulator
--
--
--isolatedArtillery : Player -> model -> Moves -> Float
--isolatedArtillery player model moves =
--    let
--        filesWithArtillery =
--            findPiecesBy (byPlayerPiecePredicate player isArtillery) model.board
--                |> toSet
--                |> Set.toList
--    in
--    List.map (\index -> adjacentFiles model index) filesWithArtillery
--        |> List.foldr (countIsolated player) 0
--        |> toFloat
--
-- A count of how many positions a player can attack on the
-- opponents side


space : Player -> Model -> Moves -> Float
space player model moves =
    let
        offset =
            if player == Red then
                0

            else
                4

        weight =
            0.1

        v =
            List.filter (\( src, dest ) -> dest.y < (8 - offset) && dest.y >= (4 - offset)) moves
                |> List.length
                |> toFloat
    in
    weight * v



-- Turn into a List of tuples


piecesByPlayer : Player -> Model -> List Piece
piecesByPlayer player model =
    let
        p : List ( Position, GameSquare )
        p =
            findPiecesBy (piecesByPlayerPredicate player) model.board

        foo : List GameSquare
        foo =
            List.map (\( x, y ) -> y) p

        getPiece x =
            case x of
                Vacant ->
                    Nothing

                Occupied _ piece ->
                    Just piece
    in
    List.filterMap getPiece foo


priceOfPiece : Piece -> Float
priceOfPiece piece =
    case piece of
        Infantry _ ->
            1

        ArmoredInfantry _ ->
            2

        Paratrooper ->
            3

        Artillery _ _ ->
            2

        HeavyArtillery _ ->
            3

        ArmoredArtillery _ ->
            3

        Headquarters ->
            500


pieceTotalPrice : Player -> Model -> Float
pieceTotalPrice player model =
    let
        pieces =
            piecesByPlayer player model
    in
    List.sum (List.map priceOfPiece pieces)


type alias PiecePredicate =
    ( Position, GameSquare ) -> Bool



-- Some useful piece predicates


piecesByPlayerPredicate : Player -> ( Position, GameSquare ) -> Bool
piecesByPlayerPredicate player ( position, gameSquare ) =
    case gameSquare of
        Vacant ->
            False

        Occupied plyr _ ->
            player == plyr


toFlatIndexedList : Board -> List ( Int, GameSquare )
toFlatIndexedList board =
    List.indexedMap (\i n -> ( i, n )) (List.concat (List.map Array.toList (Array.toList board)))


toPositionTuple : ( Int, GameSquare ) -> ( Position, GameSquare )
toPositionTuple tuple =
    let
        ( index, gameSquare ) =
            tuple
    in
    ( OnBoard { x = remainderBy 8 (index + 8), y = index // 8 }, gameSquare )


findPiecesBy : PiecePredicate -> Board -> List ( Position, GameSquare )
findPiecesBy predicate board =
    let
        listOfTuples =
            toFlatIndexedList board |> List.map toPositionTuple
    in
    List.filter predicate listOfTuples


type alias MoveFunc =
    Position -> Position



-- Our move functions


translateCoordinate : Facing -> Coordinate -> Coordinate
translateCoordinate facing coordinate =
    case facing of
        N ->
            { x = coordinate.x, y = coordinate.y - 1 }

        S ->
            { x = coordinate.x, y = coordinate.y + 1 }

        E ->
            { x = coordinate.x + 1, y = coordinate.y - 0 }

        W ->
            { x = coordinate.x - 1, y = coordinate.y - 0 }

        NW ->
            { x = coordinate.x - 1, y = coordinate.y - 1 }

        NE ->
            { x = coordinate.x + 1, y = coordinate.y - 1 }

        SE ->
            { x = coordinate.x + 1, y = coordinate.y + 1 }

        SW ->
            { x = coordinate.x - 1, y = coordinate.y + 1 }


getGameSquareByRow : Int -> Row -> Maybe GameSquare
getGameSquareByRow row columnArray =
    Array.get row columnArray


getGameSquare : Coordinate -> Board -> Maybe GameSquare
getGameSquare coordinate board =
    let
        row =
            Array.get coordinate.y board
    in
    case row of
        Just value ->
            getGameSquareByRow coordinate.x value

        _ ->
            Nothing


calculateUnderBombardmentByPlayer : Player -> Board -> Set ( Int, Int )
calculateUnderBombardmentByPlayer player board =
    let
        listOfTuples : List ( Position, GameSquare )
        listOfTuples =
            toFlatIndexedList board |> List.map toPositionTuple

        isPlayerAndArtillery : ( Position, GameSquare ) -> Bool
        isPlayerAndArtillery ( _, square ) =
            case square of
                Occupied player_ piece ->
                    player_ == player && isArtillery piece

                _ ->
                    False

        artilleryPieces : List ( Position, GameSquare )
        artilleryPieces =
            List.filter isPlayerAndArtillery listOfTuples

        bombardedCoordinates : List Coordinate
        bombardedCoordinates =
            List.concatMap bombardedCoordinatesFromPiece artilleryPieces

        bombardedPositionsAsTuples : List ( Int, Int )
        bombardedPositionsAsTuples =
            List.map (\p -> ( p.x, p.y )) bombardedCoordinates

        result =
            Set.fromList bombardedPositionsAsTuples
    in
    result


bombardedCoordinatesFromPiece : ( Position, GameSquare ) -> List Coordinate
bombardedCoordinatesFromPiece ( position, square ) =
    let
        twoSteps direction p =
            [ direction p, direction (direction p) ]
    in
    case position of
        OnBoard coordinate ->
            case square of
                Occupied _ piece ->
                    case piece of
                        Artillery _ facing ->
                            twoSteps (translateCoordinate facing) coordinate

                        HeavyArtillery facing ->
                            twoSteps (translateCoordinate facing) coordinate

                        ArmoredArtillery facing ->
                            twoSteps (translateCoordinate facing) coordinate

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


underBombardmentByOpponentOfPlayer : Player -> Coordinate -> Model -> Bool
underBombardmentByOpponentOfPlayer player coordinate model =
    if player == Blue then
        underBombardmentByPlayer Red coordinate model

    else
        underBombardmentByPlayer Blue coordinate model


underBombardmentByPlayer : Player -> Coordinate -> Model -> Bool
underBombardmentByPlayer player coordinate model =
    if player == Red then
        Set.member ( coordinate.x, coordinate.y ) model.coordinatesBombardedByRed

    else
        Set.member ( coordinate.x, coordinate.y ) model.coordinatesBombardedByBlue


isVacantAtCoordinate : Board -> Coordinate -> Bool
isVacantAtCoordinate board coordinate =
    case getGameSquare coordinate board of
        Just square ->
            case square of
                Vacant ->
                    True

                _ ->
                    False

        _ ->
            False


opponent : Player -> Player
opponent player =
    if player == Blue then
        Red

    else
        Blue


filterOutsideBoard : List Coordinate -> List Coordinate
filterOutsideBoard ps =
    let
        insideBoard : Coordinate -> Bool
        insideBoard p =
            p.x >= 0 && p.x < 8 && p.y >= 0 && p.y < 8
    in
    List.filter insideBoard ps


shortMoves : Coordinate -> List Coordinate
shortMoves coordinate =
    [ { x = coordinate.x - 1, y = coordinate.y - 1 }
    , { x = coordinate.x - 1, y = coordinate.y - 0 }
    , { x = coordinate.x - 1, y = coordinate.y + 1 }
    , { x = coordinate.x - 0, y = coordinate.y - 1 }
    , { x = coordinate.x - 0, y = coordinate.y + 1 }
    , { x = coordinate.x + 1, y = coordinate.y - 1 }
    , { x = coordinate.x + 1, y = coordinate.y - 0 }
    , { x = coordinate.x + 1, y = coordinate.y + 1 }
    ]


longMoves : Coordinate -> List Coordinate
longMoves coordinate =
    shortMoves coordinate
        ++ [ { x = coordinate.x - 2, y = coordinate.y - 2 }
           , { x = coordinate.x - 2, y = coordinate.y - 0 }
           , { x = coordinate.x - 2, y = coordinate.y + 2 }
           , { x = coordinate.x - 0, y = coordinate.y - 2 }
           , { x = coordinate.x - 0, y = coordinate.y + 2 }
           , { x = coordinate.x + 2, y = coordinate.y - 2 }
           , { x = coordinate.x + 2, y = coordinate.y - 0 }
           , { x = coordinate.x + 2, y = coordinate.y + 2 }
           ]


isVacant : GameSquare -> Bool
isVacant gameSquare =
    gameSquare == Vacant


allAttackFree : Player -> Position -> Position -> Position -> Board -> Bool
allAttackFree player one two three board =
    if not (checkForAttacks player one board) then
        if not (checkForAttacks player two board) then
            not (checkForAttacks player three board)

        else
            False

    else
        False


filteringVacant : Board -> Coordinate -> Maybe Coordinate
filteringVacant board coordinate =
    if isVacantAtCoordinate board coordinate then
        Just coordinate

    else
        Nothing


shortMovePositions : Player -> Coordinate -> Model -> List Coordinate
shortMovePositions player coordinate model =
    List.filter (\x -> underBombardmentByOpponentOfPlayer player x model)
        (List.filterMap (filteringVacant model.board) (shortMoves coordinate))


longMovePositions : Player -> Coordinate -> Model -> List Coordinate
longMovePositions player coordinate model =
    List.filter (\x -> underBombardmentByOpponentOfPlayer player x model)
        (List.filterMap (filteringVacant model.board) (longMoves coordinate))


unoccupiedSpacesNotUnderBombardmentByOpponent : Player -> Model -> List Coordinate
unoccupiedSpacesNotUnderBombardmentByOpponent player model =
    List.filter (\x -> underBombardmentByOpponentOfPlayer player x model)
        (List.filterMap (filteringVacant model.board) (allUnoccupiedSpaces model))


allUnoccupiedSpaces : Model -> List Coordinate
allUnoccupiedSpaces model =
    let
        allPositions : List Coordinate
        allPositions =
            let
                foo x =
                    List.map (\y -> { x = x, y = y }) (List.range 0 7)
            in
            List.concatMap foo (List.range 0 7)

        allSquares : List ( Coordinate, GameSquare )
        allSquares =
            let
                foo : Coordinate -> Maybe ( Coordinate, GameSquare )
                foo c =
                    case ( c, getGameSquare c model.board ) of
                        ( _, Just yy ) ->
                            Just ( c, yy )

                        _ ->
                            Nothing
            in
            List.filterMap foo allPositions
    in
    List.map (\( p, _ ) -> p)
        (List.filter (\( _, square ) -> isVacant square) allSquares)


paratrooperMovePositions : Coordinate -> Player -> Model -> List Coordinate
paratrooperMovePositions coordinate player model =
    if onHomeRow player coordinate then
        unoccupiedSpacesNotUnderBombardmentByOpponent player model

    else
        -- TODO: disallow engaged to engaged moves
        shortMovePositions player coordinate model


onHomeRow : Player -> Coordinate -> Bool
onHomeRow player coordinate =
    (coordinate.y == 0 && player == Red)
        || (coordinate.y == 7 && player == Blue)



--walk : Player -> Position -> Board -> List Position -> MoveFunc -> List Position
--walk player position board accum moveFunc =
--    let
--        nextPosition =
--            moveFunc position
--
--        maybeSquare =
--            getGameSquare (moveFunc position) board
--    in
--    case maybeSquare of
--        Just value ->
--            case value of
--                Vacant ->
--                    walk player nextPosition board (accum ++ [ nextPosition ]) moveFunc
--
--                Occupied consideringPlayer piece ->
--                    if consideringPlayer == opponent player then
--                        accum ++ [ nextPosition ]
--
--                    else
--                        accum
--
--        Nothing ->
--            accum


setGameSquare : Coordinate -> GameSquare -> Board -> Board
setGameSquare coordinate gameSquare board =
    let
        maybeColumn =
            Array.get coordinate.y board
    in
    case maybeColumn of
        Just column ->
            let
                maybeSquare =
                    Array.get coordinate.x column
            in
            case maybeSquare of
                Just destSquare ->
                    Array.set coordinate.y (Array.set coordinate.x gameSquare column) board

                Nothing ->
                    board

        Nothing ->
            board


coordinateToString : Coordinate -> String
coordinateToString coords =
    String.fromInt coords.x ++ " " ++ String.fromInt coords.y


logList : List Coordinate -> List Coordinate
logList list =
    let
        log =
            List.map (\n -> Debug.log "move" (coordinateToString n)) list
    in
    list



--walkDirection : Player -> Piece -> Position -> Board -> MoveFunc -> Bool
--walkDirection player piece position board moveFunc =
--    let
--        nextPosition =
--            moveFunc position
--
--        maybeSquare =
--            getGameSquare (moveFunc position) board
--    in
--    case maybeSquare of
--        Just value ->
--            case value of
--                Vacant ->
--                    walkDirection player piece nextPosition board moveFunc
--
--                Occupied consideringPlayer pce ->
--                    if (consideringPlayer == opponent player) && pce == piece then
--                        True
--
--                    else
--                        False
--
--        Nothing ->
--            False
--lazilyWalkDirection : Player -> Piece -> Position -> Board -> MoveFunc -> Bool -> Bool
--lazilyWalkDirection player piece position board moveFunc lastEval =
--    if lastEval then
--        True
--
--    else
--        walkDirection player piece position board moveFunc
--facesAttackFrom : Player -> Piece -> List MoveFunc -> Position -> Board -> Bool
--facesAttackFrom player piece moveFuncs position board =
--    List.foldr (lazilyWalkDirection player piece position board) False moveFuncs


checkForAttacks player position board =
    -- TODO: implement
    False



--(List.map (isOpponentPiece player Knight board) (knightMoves position) |> List.any (\v -> v))
--    || (isOpponentPiece player Pawn board (Position (position.x - 1) (position.y + yDirection))
--            || isOpponentPiece player Pawn board (Position (position.x + 1) (position.y + yDirection))
--       )
--    || (List.map (isOpponentPiece player King board) (kingMoves position) |> List.any (\v -> v))
--    || facesAttackFrom player Rook rookMovements position board
--    || facesAttackFrom player Bishop bishopMovements position board
--    || facesAttackFrom player Queen queenMovements position board


isOpponentPiece : Player -> Piece -> Board -> Coordinate -> Bool
isOpponentPiece player piece board coordinate =
    let
        other =
            opponent player

        maybeGameSquare =
            getGameSquare coordinate board
    in
    case maybeGameSquare of
        Just gameSquare ->
            case gameSquare of
                Occupied plr pce ->
                    (plr == other) && (pce == piece)

                _ ->
                    False

        Nothing ->
            False


validMovesPerPiece : Model -> ( Position, GameSquare ) -> List Move
validMovesPerPiece model tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    validMoves position model
        |> List.map (\dest -> ( position, dest ))


allAvailableMoves : Player -> Model -> List Move
allAvailableMoves player model =
    let
        pieces =
            findPiecesBy (piecesByPlayerPredicate player) model.board
    in
    List.map (validMovesPerPiece model) pieces
        |> List.concat


redsHomeRow =
    List.map (\x -> { x = x, y = 0 }) (List.range 0 7)


bluesHomeRow =
    List.map (\x -> { x = x, y = 7 }) (List.range 0 7)


homeRow : Player -> List Coordinate
homeRow player =
    case player of
        Red ->
            redsHomeRow

        Blue ->
            bluesHomeRow


validMoves : Position -> Model -> List Coordinate
validMoves position model =
    case position of
        Reinforcement player ->
            homeRow player

        OnBoard coordinate ->
            case getGameSquare coordinate model.board of
                Nothing ->
                    []

                Just square ->
                    case square of
                        Vacant ->
                            []

                        Occupied player piece ->
                            let
                                rawMoves : List Coordinate
                                rawMoves =
                                    case piece of
                                        ArmoredInfantry _ ->
                                            longMoves coordinate

                                        ArmoredArtillery _ ->
                                            longMoves coordinate

                                        Paratrooper ->
                                            paratrooperMovePositions coordinate player model

                                        _ ->
                                            shortMoves coordinate
                            in
                            rawMoves |> filterOutsideBoard |> filterOnlyVacant model |> filterDoNotEnterBombarded player model



--|> filterDoNotGoFromEngagedToEngaged


filterOnlyVacant : Model -> List Coordinate -> List Coordinate
filterOnlyVacant model coordinates =
    let
        foo p =
            case p of
                Nothing ->
                    False

                Just p2 ->
                    isVacant p2
    in
    List.filter (\x -> foo (getGameSquare x model.board)) coordinates


filterDoNotEnterBombarded : Player -> Model -> List Coordinate -> List Coordinate
filterDoNotEnterBombarded player model coordinates =
    let
        bombarded =
            case player of
                Red ->
                    model.coordinatesBombardedByBlue

                Blue ->
                    model.coordinatesBombardedByRed
    in
    List.filter (\p -> not (Set.member ( p.x, p.y ) bombarded)) coordinates


updateBoard : Board -> Model -> Model
updateBoard board model =
    { model | board = board }


dropMaybe : Maybe GameSquare -> GameSquare
dropMaybe gameSquare =
    case gameSquare of
        Just value ->
            value

        _ ->
            Vacant


updateGame : Move -> Model -> Model
updateGame ( src, dest ) model =
    let
        destSquare =
            getGameSquare dest model.board |> dropMaybe

        srcSquare =
            getGameSquare src model.board |> dropMaybe

        updatedBoard =
            setGameSquare dest srcSquare model.board
                |> setGameSquare src Vacant
    in
    updateBoard updatedBoard model
        |> nextTurn



--
--setVacant : Player -> Int -> Board -> Board
--setVacant player x board =
--    let
--        y =
--                   if player == Red then
--                0
--
--            else
--                7
--    in
--    setGameSquare (Position x y) Vacant board
--setAdjacentSquare : Int -> Position -> GameSquare -> Board -> Board
--setAdjacentSquare direction position gameSquare board =
--    setGameSquare (Position (position.x + direction) position.y) gameSquare board


nextTurn : Model -> Model
nextTurn model =
    let
        newTurn =
            case model.turn of
                BluesTurn First ->
                    BluesTurn Second

                BluesTurn Second ->
                    BluesTurn Third

                BluesTurn Third ->
                    RedsTurn First

                RedsTurn First ->
                    RedsTurn Second

                RedsTurn Second ->
                    RedsTurn Third

                RedsTurn Third ->
                    BluesTurn First
    in
    { model | turn = newTurn }


type Player
    = Blue
    | Red


type Piece
    = Infantry InfantryDivision
    | ArmoredInfantry ArmoredInfantryDivision
    | Paratrooper
    | Artillery ArtilleryDivision Facing
    | HeavyArtillery Facing
    | ArmoredArtillery Facing
    | Headquarters


type InfantryDivision
    = Infantry1
    | Infantry2
    | Infantry3
    | Infantry4
    | Infantry5
    | Infantry6
    | Infantry7


type ArmoredInfantryDivision
    = ArmoredInfantry1
    | ArmoredInfantry2
    | ArmoredInfantry3


type ArtilleryDivision
    = Artillery1
    | Artillery2
    | Artillery3


isArtillery : Piece -> Bool
isArtillery p =
    case p of
        HeavyArtillery _ ->
            True

        ArmoredArtillery _ ->
            True

        Artillery _ _ ->
            True

        _ ->
            False


type Facing
    = N
    | S
    | E
    | W
    | NW
    | NE
    | SW
    | SE


type GameSquare
    = Vacant
    | Occupied Player Piece


type alias Row =
    Array GameSquare


type alias Board =
    Array Row


type Position
    = OnBoard Coordinate
    | Reinforcement Player


type alias Coordinate =
    { x : Int, y : Int }


type alias Move =
    -- from a position (reinforcement or on board) to a coordinate on the board
    ( Position, Coordinate )


type alias Moves =
    List Move


type SubTurn
    = First
    | Second
    | Third


type Turn
    = BluesTurn SubTurn
    | RedsTurn SubTurn


nextMove : Model -> Task x (Maybe Move)
nextMove model =
    let
        ( score, move ) =
            List.map (\m -> ( applyMove m model, m )) (nextMoves model)
                |> List.foldr executeMin ( -1000.0, dummyMove )
    in
    if move == dummyMove then
        Task.succeed Nothing

    else
        Task.succeed (Just move)


executeMin : ( Model, Move ) -> ( Float, Move ) -> ( Float, Move )
executeMin ( model, move ) ( bestScore, bestMove ) =
    let
        newBeta =
            abMin 1 bestScore 1000 model
    in
    if newBeta > bestScore then
        ( newBeta, move )

    else
        ( bestScore, bestMove )


dummyMove : Move
dummyMove =
    ( OnBoard { x = 0, y = 0 }, { x = 0, y = 0 } )


applyMove : Move -> Model -> Model
applyMove move model =
    updateGame move model


logSize : Moves -> Moves
logSize moves =
    let
        _ =
            Debug.log "MOVE COUNT: " (String.fromInt (List.length moves))
    in
    moves


type alias MoveToCompare =
    { src : Position
    , dest : Position
    , srcSquare : Maybe GameSquare
    , destSquare : Maybe GameSquare
    }


moveComparison : MoveToCompare -> MoveToCompare -> Order
moveComparison a b =
    let
        aDestSquare =
            Maybe.withDefault Vacant a.destSquare

        bDestSquare =
            Maybe.withDefault Vacant b.destSquare
    in
    case aDestSquare of
        Vacant ->
            case bDestSquare of
                Vacant ->
                    EQ

                _ ->
                    LT

        _ ->
            case bDestSquare of
                Vacant ->
                    GT

                _ ->
                    EQ


sortByAttacks : Player -> Model -> Moves -> Moves
sortByAttacks player model moves =
    let
        targetSquares =
            List.map (\( src, dest ) -> MoveToCompare src dest (getGameSquare src model.board) (getGameSquare dest model.board)) moves
    in
    List.sortWith moveComparison targetSquares
        |> List.map (\mv -> ( mv.src, mv.dest ))


nextMoves : Model -> Moves
nextMoves model =
    let
        player =
            case model.turn of
                RedsTurn _ ->
                    Red

                BluesTurn _ ->
                    Blue
    in
    allAvailableMoves player model |> sortByAttacks player model


stripMaybe : Maybe Move -> Move
stripMaybe list =
    case list of
        Just value ->
            value

        Nothing ->
            dummyMove


stripMaybeList : Maybe Moves -> Moves
stripMaybeList list =
    case list of
        Just value ->
            value

        Nothing ->
            []


abMinHelper : Int -> Float -> Float -> Model -> Moves -> Float
abMinHelper depth alpha beta model moves =
    let
        listLength =
            List.length moves

        move =
            List.head moves |> stripMaybe

        newV =
            if listLength > 0 then
                abMax (depth - 1) alpha beta (applyMove move model)

            else
                beta

        newB =
            Basics.min beta newV
    in
    if listLength == 0 then
        beta

    else if newB <= alpha then
        alpha

    else
        abMinHelper depth alpha newB model (List.tail moves |> stripMaybeList)


abMin : Int -> Float -> Float -> Model -> Float
abMin depth alpha beta model =
    if depth == 0 then
        eval model

    else
        abMinHelper depth alpha beta model (nextMoves model)


abMaxHelper : Int -> Float -> Float -> Model -> Moves -> Float
abMaxHelper depth alpha beta model moves =
    let
        listLength =
            List.length moves

        move =
            List.head moves |> stripMaybe

        newV =
            if listLength > 0 then
                abMin (depth - 1) alpha beta (applyMove move model)

            else
                alpha

        newA =
            Basics.max alpha newV
    in
    if listLength == 0 then
        alpha

    else if beta <= newA then
        beta

    else
        abMaxHelper depth newA beta model (List.tail moves |> stripMaybeList)


abMax : Int -> Float -> Float -> Model -> Float
abMax depth alpha beta model =
    if depth == 0 then
        eval model

    else
        abMaxHelper depth alpha beta model (nextMoves model)


view : Model -> Document Msg
view model =
    { title = "Elm GHQ"
    , body =
        [ div
            [ style "position" "relative"
            , style "width" (px (squareSize * 8))
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ div [ style "position" "relative", style "height" (px (squareSize * 2)) ] [ viewReinforcements model Red ]
            , div [ style "position" "relative", style "height" (px (squareSize * 8)) ] [ viewBoard model ]
            , div [ style "position" "relative", style "height" (px (squareSize * 2)) ] [ viewReinforcements model Blue ]
            ]
        ]
    }


viewReinforcements : Model -> Player -> Html Msg
viewReinforcements model player =
    let
        pieces =
            case player of
                Red ->
                    model.redReinforcements

                Blue ->
                    model.blueReinforcements

        foo x piece =
            renderPiece model player piece (Reinforcement player)
    in
    div [] (List.indexedMap foo pieces)


viewRow : Row -> Html Msg
viewRow row =
    div [] (List.map viewSquare row)


viewBoard : Model -> Html Msg
viewBoard model =
    div []
        (List.map viewRow (Array.toList model.board))
