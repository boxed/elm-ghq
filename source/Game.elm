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



-- TODO: implement infantry capture
-- TODO: implement artillery capture


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
    , hasMoved : Set CoordinateTuple
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
    ( updateCalculatedBoardData
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
        , hasMoved = Set.empty
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


updateCalculatedBoardData : Model -> Model
updateCalculatedBoardData model =
    model
        |> updatePotentialMoves
        |> updateBombardmentPositions


startEngine : Posix -> Msg
startEngine time =
    StartEngine


executeEngine : Model -> Cmd Msg
executeEngine model =
    Task.perform ComputerMove (nextMove model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PositionClicked position ->
            ( processClicked position model |> updateCalculatedBoardData, Cmd.none )

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


setSelectedPosition : Position -> Model -> Model
setSelectedPosition position model =
    { model | selected = SelectedPosition position } |> updateCalculatedBoardData


squareSize =
    64


determineMoves : Position -> Model -> Set ( Int, Int )
determineMoves position model =
    let
        toTuple : Coordinate -> ( Int, Int )
        toTuple coordinate =
            ( coordinate.x, coordinate.y )
    in
    validMoves position model |> List.map toTuple |> Set.fromList


updatePotentialMoves : Model -> Model
updatePotentialMoves model =
    case model.selected of
        SelectedPosition position ->
            { model | potentialMoves = determineMoves position model }

        NoSelection ->
            { model | potentialMoves = Set.empty }


processMove : Position -> Coordinate -> Model -> Model
processMove src dest model =
    let
        updatedGameModel =
            applyMove ( src, dest ) model
    in
    { updatedGameModel | computerThinking = False }
        |> clearSelected
        |> updateCalculatedBoardData
        |> markHasMoved dest


markHasMoved : Coordinate -> Model -> Model
markHasMoved coordinate model =
    { model | hasMoved = Set.insert ( coordinate.x, coordinate.y ) model.hasMoved }


processClicked : Position -> Model -> Model
processClicked position model =
    if model.selected == SelectedPosition position then
        clearSelected model

    else
        case position of
            Reinforcement _ _ ->
                setSelectedPosition position model

            OnBoard coordinate ->
                if Set.member ( coordinate.x, coordinate.y ) model.hasMoved then
                    model

                else
                    case getSquare coordinate model.board of
                        Just Vacant ->
                            if Set.member ( coordinate.x, coordinate.y ) model.potentialMoves then
                                case model.selected of
                                    NoSelection ->
                                        model

                                    SelectedPosition selectedPosition ->
                                        processMove selectedPosition coordinate model |> clearSelected

                            else
                                model

                        Just (Occupied player _) ->
                            if player == Blue then
                                setSelectedPosition position model

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


viewTile : Model -> Position -> Square -> Html Msg
viewTile model position square =
    let
        bombardmentBackgroundClass =
            case position of
                OnBoard coordinate ->
                    if underBombardmentByPlayer Red coordinate model then
                        "red-bombardment"

                    else if underBombardmentByPlayer Blue coordinate model then
                        "blue-bombardment"

                    else
                        ""

                _ ->
                    ""

        hasMovedBackgroundClass =
            case position of
                OnBoard coordinate ->
                    if Set.member ( coordinate.x, coordinate.y ) model.hasMoved then
                        "has-moved"

                    else
                        ""

                _ ->
                    ""

        borderClass =
            if model.selected == SelectedPosition position then
                "selected"

            else
                case position of
                    OnBoard c ->
                        if Set.member ( c.x, c.y ) model.potentialMoves then
                            "potential-move"

                        else
                            "plain-tile"

                    _ ->
                        "plain-tile"

        ( content, textColor ) =
            case square of
                Vacant ->
                    ( [ text "o" ], "transparent" )

                Occupied player piece ->
                    ( [ text (pieceToCharacter piece) ]
                    , if player == Blue then
                        "blue"

                      else
                        "red"
                    )
    in
    div
        [ onClick (PositionClicked position)
        , style "cursor" "pointer"

        -- TODO: implement images
        --, Html.Attributes.src (chooseImage player piece)
        , style "width" (px squareSize)
        , style "height" (px squareSize)
        , style "text-align" "center"
        , style "display" "inline-block"
        , style "color" textColor
        , class borderClass
        , class hasMovedBackgroundClass
        , class bombardmentBackgroundClass
        ]
        content



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


type alias File =
    List ( Position, Square )


type alias EvaluatorFunc =
    Player -> Model -> Moves -> Float



-- An extensible evaluation model


evaluators : List EvaluatorFunc
evaluators =
    [ pieceTotalPrice

    --, ( mobility, 0.1 )
    , space

    --, ( isolatedArtillery, -0.5 )
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


occupiedBy : Player -> Board -> Coordinate -> Maybe Position
occupiedBy player board coordinate =
    let
        maybeSquare =
            getSquare coordinate board
    in
    case maybeSquare of
        Just value ->
            case value of
                Occupied p _ ->
                    if p == player then
                        Just (OnBoard coordinate)

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
--    findPiecesBy (\( position, square ) -> position.x == index) model.board
--        |> List.sortWith yComparison
--
--
--adjacentFiles : Model -> Int -> ( File, File )
--adjacentFiles model fileIndex =
--    ( file model (fileIndex - 1), file model (fileIndex + 1) )
--
--
--toSetHelper : List ( Position, Square ) -> Set Int -> Set Int
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
--toSet : List ( Position, Square ) -> Set Int
--toSet list =
--    toSetHelper list Set.empty
--
--
--filterForPawns player piece ( position, square ) =
--    case square of
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
        p : List ( Position, Square )
        p =
            findPiecesBy (piecesByPlayerPredicate player) model.board

        foo : List Square
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


pieceTotalPrice : Player -> Model -> Moves -> Float
pieceTotalPrice player model moves =
    let
        pieces =
            piecesByPlayer player model
    in
    List.sum (List.map priceOfPiece pieces)


type alias PiecePredicate =
    ( Position, Square ) -> Bool



-- Some useful piece predicates


piecesByPlayerPredicate : Player -> ( Position, Square ) -> Bool
piecesByPlayerPredicate player ( position, square ) =
    case square of
        Vacant ->
            False

        Occupied plyr _ ->
            player == plyr


toFlatIndexedList : Board -> List ( Int, Square )
toFlatIndexedList board =
    List.indexedMap (\i n -> ( i, n )) (List.concat (List.map Array.toList (Array.toList board)))


toPositionTuple : ( Int, Square ) -> ( Position, Square )
toPositionTuple ( index, square ) =
    ( OnBoard { x = remainderBy 8 (index + 8), y = index // 8 }, square )


findPiecesBy : PiecePredicate -> Board -> List ( Position, Square )
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


getSquareByRow : Int -> Row -> Maybe Square
getSquareByRow row columnArray =
    Array.get row columnArray


getSquare : Coordinate -> Board -> Maybe Square
getSquare coordinate board =
    let
        row =
            Array.get coordinate.y board
    in
    case row of
        Just value ->
            getSquareByRow coordinate.x value

        _ ->
            Nothing


calculateUnderBombardmentByPlayer : Player -> Board -> Set ( Int, Int )
calculateUnderBombardmentByPlayer player board =
    let
        listOfTuples : List ( Position, Square )
        listOfTuples =
            toFlatIndexedList board |> List.map toPositionTuple

        isPlayerAndArtillery : ( Position, Square ) -> Bool
        isPlayerAndArtillery ( _, square ) =
            case square of
                Occupied player_ piece ->
                    player_ == player && isArtillery piece

                _ ->
                    False

        artilleryPieces : List ( Position, Square )
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


bombardedCoordinatesFromPiece : ( Position, Square ) -> List Coordinate
bombardedCoordinatesFromPiece ( position, square ) =
    let
        twoSteps direction p =
            [ direction p, direction (direction p) ]

        threeSteps direction p =
            [ direction p, direction (direction p), direction (direction (direction p)) ]
    in
    case position of
        OnBoard coordinate ->
            case square of
                Occupied _ piece ->
                    case piece of
                        Artillery _ facing ->
                            twoSteps (translateCoordinate facing) coordinate

                        ArmoredArtillery facing ->
                            twoSteps (translateCoordinate facing) coordinate

                        HeavyArtillery facing ->
                            threeSteps (translateCoordinate facing) coordinate

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
    case getSquare coordinate board of
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


longMoves : Model -> Coordinate -> List Coordinate
longMoves model coordinate =
    let
        validShortMoves =
            shortMoves coordinate |> filterOnlyVacant model

        foo : Coordinate -> Coordinate
        foo c =
            let
                diffX =
                    c.x - coordinate.x

                diffY =
                    c.y - coordinate.y
            in
            { x = coordinate.x + diffX * 2, y = coordinate.y + diffY * 2 }

        validLongMoves : List Coordinate
        validLongMoves =
            List.map foo validShortMoves
    in
    validShortMoves ++ validLongMoves


isVacant : Square -> Bool
isVacant square =
    square == Vacant


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

        allSquares : List ( Coordinate, Square )
        allSquares =
            let
                foo : Coordinate -> Maybe ( Coordinate, Square )
                foo c =
                    case ( c, getSquare c model.board ) of
                        ( _, Just yy ) ->
                            Just ( c, yy )

                        _ ->
                            Nothing
            in
            List.filterMap foo allPositions
    in
    List.map (\( p, _ ) -> p)
        (List.filter (\( _, square ) -> isVacant square) allSquares)


paratrooperMoves : Coordinate -> Player -> Model -> List Coordinate
paratrooperMoves coordinate player model =
    if onHomeRow player coordinate then
        unoccupiedSpacesNotUnderBombardmentByOpponent player model

    else
        shortMoves coordinate


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
--            getSquare (moveFunc position) board
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


setSquare : Coordinate -> Square -> Model -> Model
setSquare coordinate square model =
    let
        board =
            model.board

        maybeColumn =
            Array.get coordinate.y board

        newBoard =
            case maybeColumn of
                Just column ->
                    let
                        maybeSquare =
                            Array.get coordinate.x column
                    in
                    case maybeSquare of
                        Just destSquare ->
                            Array.set coordinate.y (Array.set coordinate.x square column) board

                        Nothing ->
                            board

                Nothing ->
                    board
    in
    { model | board = newBoard }


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
--            getSquare (moveFunc position) board
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

        maybeSquare =
            getSquare coordinate board
    in
    case maybeSquare of
        Just square ->
            case square of
                Occupied plr pce ->
                    (plr == other) && (pce == piece)

                _ ->
                    False

        Nothing ->
            False


validMovesPerPiece : Model -> ( Position, Square ) -> List Move
validMovesPerPiece model ( position, square ) =
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
        Reinforcement player _ ->
            homeRow player |> filterOnlyVacant model

        OnBoard coordinate ->
            case getSquare coordinate model.board of
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
                                            longMoves model coordinate

                                        ArmoredArtillery _ ->
                                            longMoves model coordinate

                                        Paratrooper ->
                                            paratrooperMoves coordinate player model

                                        _ ->
                                            shortMoves coordinate
                            in
                            rawMoves
                                |> filterOutsideBoard
                                |> filterOnlyVacant model
                                |> filterDoNotEnterBombarded player model
                                |> filterDoNotGoFromEngagedToEngaged model player piece coordinate


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
    List.filter (\x -> foo (getSquare x model.board)) coordinates


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


filterDoNotGoFromEngagedToEngaged : Model -> Player -> Piece -> Coordinate -> List Coordinate -> List Coordinate
filterDoNotGoFromEngagedToEngaged model player piece coordinate coordinates =
    if isInfantry piece && isEngagedPosition model player coordinate then
        List.filter (\c -> not (isEngagedPosition model player c)) coordinates

    else
        coordinates


isEngagedPosition : Model -> Player -> Coordinate -> Bool
isEngagedPosition model player coordinate =
    let
        orthogonalNeighbors : List Coordinate
        orthogonalNeighbors =
            getOrthogonalNeighbors coordinate |> filterOutsideBoard

        checkArtillery : Coordinate -> Bool
        checkArtillery c =
            case getSquare c model.board of
                Nothing ->
                    False

                Just Vacant ->
                    False

                Just (Occupied player_ piece) ->
                    player_ == opponent player && isInfantry piece
    in
    List.any checkArtillery orthogonalNeighbors


getOrthogonalNeighbors : Coordinate -> List Coordinate
getOrthogonalNeighbors coordinate =
    [ translateCoordinate N coordinate
    , translateCoordinate E coordinate
    , translateCoordinate W coordinate
    , translateCoordinate S coordinate
    ]


dropMaybe : Maybe Square -> Square
dropMaybe square =
    case square of
        Just value ->
            value

        _ ->
            Vacant


applyMove : Move -> Model -> Model
applyMove ( from, to ) model =
    let
        newSquare =
            case from of
                OnBoard fromCoordinate ->
                    getSquare fromCoordinate model.board |> dropMaybe

                Reinforcement player piece ->
                    Occupied player piece
    in
    model
        |> setVacant from
        |> setSquare to newSquare
        |> nextTurn


setVacant : Position -> Model -> Model
setVacant position model =
    case position of
        Reinforcement player piece ->
            case player of
                Red ->
                    { model | redReinforcements = List.filter (\x -> x /= piece) model.redReinforcements }

                Blue ->
                    { model | blueReinforcements = List.filter (\x -> x /= piece) model.blueReinforcements }

        OnBoard coordinate ->
            setSquare coordinate Vacant model



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
--    setSquare (Position x y) Vacant board
--setAdjacentSquare : Int -> Position -> Square -> Board -> Board
--setAdjacentSquare direction position square board =
--    setSquare (Position (position.x + direction) position.y) square board


nextTurn : Model -> Model
nextTurn model =
    case model.turn of
        BluesTurn First ->
            { model | turn = BluesTurn Second }

        BluesTurn Second ->
            { model | turn = BluesTurn Third }

        BluesTurn Third ->
            { model | turn = RedsTurn First, hasMoved = Set.empty }

        RedsTurn First ->
            { model | turn = RedsTurn Second }

        RedsTurn Second ->
            { model | turn = RedsTurn Third }

        RedsTurn Third ->
            { model | turn = BluesTurn First, hasMoved = Set.empty }


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


isInfantry : Piece -> Bool
isInfantry p =
    case p of
        Infantry _ ->
            True

        ArmoredInfantry _ ->
            True

        Paratrooper ->
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


type Square
    = Vacant
    | Occupied Player Piece


type alias Row =
    Array Square


type alias Board =
    Array Row


type Position
    = OnBoard Coordinate
    | Reinforcement Player Piece


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


logSize : Moves -> Moves
logSize moves =
    let
        _ =
            Debug.log "MOVE COUNT: " (String.fromInt (List.length moves))
    in
    moves


type alias MoveToCompare =
    { dest : Position
    , destSquare : Maybe Square
    }


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
    allAvailableMoves player model


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


viewReinforcements : Model -> Player -> Html Msg
viewReinforcements model player =
    let
        pieces =
            case player of
                Red ->
                    model.redReinforcements

                Blue ->
                    model.blueReinforcements

        foo piece =
            viewTile model (Reinforcement player piece) (Occupied player piece)
    in
    div [] (List.map foo pieces)


viewRow : Model -> Int -> Row -> Html Msg
viewRow model y row =
    let
        foo : Int -> Square -> Html Msg
        foo x square =
            viewTile model (OnBoard { x = x, y = y }) square
    in
    div [] (List.indexedMap foo (Array.toList row))


viewBoard : Model -> Html Msg
viewBoard model =
    div []
        (List.indexedMap (viewRow model) (Array.toList model.board))



-- TODO: debug data: show hasMoved


view : Model -> Document Msg
view model =
    { title = "Elm GHQ"
    , body =
        [ div
            [ style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ div [] [ viewReinforcements model Red ]
            , div [ style "margin" "20px" ] [ viewBoard model ]
            , div [] [ viewReinforcements model Blue ]
            ]
        ]
    }
