module Main exposing (..)

import Browser
import Browser.Events
import Char exposing (isDigit, isLower, isUpper)
import Debug exposing (log)
import Html exposing (Html, div, h1, h5, span)
import Html.Attributes
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { board : Board
    , carConfig : BlockConfig
    , carPosition : List BlockPosition
    , obstacleConfig : BlockConfig
    , obstacles : List (List BlockPosition)
    , gameOver : Bool
    , millisElapsed : Float
    , ticks : Int
    , level : Int
    , points : Int
    }


blockSize =
    10


boardBlockHeight =
    40


boardBlockWidth =
    4


secondsBetwenLevels =
    5


getPixelSize : Int -> Int
getPixelSize blocksNumber =
    blocksNumber * blockSize


type alias Board =
    { height : Int
    , width : Int
    , border : String
    }


defaultBoard : Board
defaultBoard =
    { height = boardBlockHeight
    , width = boardBlockWidth
    , border = "grey"
    }


type alias BlockConfig =
    { height : Int
    , width : Int
    , color : String
    , border : String
    }


type alias BlockPosition =
    { x : Int
    , y : Int
    }


initialCarPosition : List BlockPosition
initialCarPosition =
    [ { x = 0, y = boardBlockHeight - 4 }
    , { x = 1, y = boardBlockHeight - 4 }
    , { x = 0, y = boardBlockHeight - 3 }
    , { x = 1, y = boardBlockHeight - 3 }
    , { x = 0, y = boardBlockHeight - 2 }
    , { x = 1, y = boardBlockHeight - 2 }
    ]


type ObstacleLocation
    = LeftSide
    | RightSide


obstacleLocation : Random.Generator ObstacleLocation
obstacleLocation =
    Random.uniform LeftSide [ RightSide ]


leftObstacle : List BlockPosition
leftObstacle =
    [ { x = 0, y = 0 }
    , { x = 1, y = 0 }
    ]


rightObstacle : List BlockPosition
rightObstacle =
    [ { x = 2, y = 0 }
    , { x = 3, y = 0 }
    ]


initModel : Model
initModel =
    { board = defaultBoard
    , carConfig =
        { height = blockSize
        , width = blockSize
        , color = "black"
        , border = "grey"
        }
    , carPosition = initialCarPosition
    , obstacleConfig =
        { height = blockSize
        , width = blockSize
        , color = "green"
        , border = "grey"
        }
    , obstacles = [ leftObstacle ]
    , gameOver = False
    , ticks = 0
    , millisElapsed = 0
    , level = 0
    , points = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Noop
    | Tick Time.Posix
    | AnimationTick Float
    | NextLevel Time.Posix
    | KeyPressed Key
    | NewObstacle ObstacleLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed direction ->
            case direction of
                Left ->
                    ( moveHorizontally model -2 |> checkGameOver, Cmd.none )

                Right ->
                    ( moveHorizontally model 2 |> checkGameOver, Cmd.none )

                Space ->
                    ( initModel, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        Tick _ ->
            let
                ticks =
                    model.ticks + 1
            in
            ( { model | ticks = ticks } |> moveObstacles |> checkGameOver
            , if shouldGenerateObstacle ticks then
                Random.generate NewObstacle obstacleLocation

              else
                Cmd.none
            )

        ---- TODO make smoother animation
        AnimationTick deltaMillis ->
            let
                millisElapsed =
                    deltaMillis + model.millisElapsed

                newTicks =
                    round (millisElapsed / toFloat (100 - model.level * 10))

                newModel =
                    { model | ticks = newTicks, millisElapsed = millisElapsed }
            in
            ( if newTicks > model.ticks then
                newModel |> moveObstacles |> checkGameOver

              else
                newModel
            , Cmd.none
            )

        NextLevel _ ->
            ( { model | level = model.level + 1 }, Cmd.none )

        NewObstacle location ->
            let
                obstacle =
                    case location of
                        LeftSide ->
                            leftObstacle

                        RightSide ->
                            rightObstacle
            in
            ( { model | obstacles = obstacle :: model.obstacles }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


shouldGenerateObstacle ticks =
    remainderBy 10 ticks == 0


blocksAreOverlapping : Model -> Bool
blocksAreOverlapping model =
    let
        blocksOverlap block1 block2 =
            block1.x == block2.x && block1.y == block2.y

        carIsHit : BlockPosition -> Bool
        carIsHit block =
            List.any (blocksOverlap block) model.carPosition
    in
    flatten2D model.obstacles |> List.any carIsHit


checkGameOver : Model -> Model
checkGameOver model =
    if blocksAreOverlapping model then
        { model | gameOver = True }

    else
        model


generateObstacle : ObstacleLocation -> List BlockPosition
generateObstacle location =
    case location of
        LeftSide ->
            leftObstacle

        RightSide ->
            rightObstacle


moveObstacles : Model -> Model
moveObstacles model =
    let
        move : List BlockPosition -> List BlockPosition
        move positions =
            List.map (moveBlockVertically 1) positions

        movedObstacles =
            List.map move model.obstacles |> List.filter isInsideBoard

        pointsEarned =
            List.length model.obstacles - List.length movedObstacles
    in
    { model | obstacles = movedObstacles, points = model.points + pointsEarned }


moveBlockHorizontally : Int -> BlockPosition -> BlockPosition
moveBlockHorizontally blocksToMove position =
    { position | x = position.x + blocksToMove }


moveBlockVertically : Int -> BlockPosition -> BlockPosition
moveBlockVertically blocksToMove position =
    { position | y = position.y + blocksToMove }


isBlockInsideBoard : BlockPosition -> Bool
isBlockInsideBoard position =
    not (position.x < 0 || position.x > boardBlockWidth || position.y < 0 || position.y > boardBlockHeight)


isInsideBoard : List BlockPosition -> Bool
isInsideBoard positions =
    List.all isBlockInsideBoard positions


moveHorizontally : Model -> Int -> Model
moveHorizontally model blocksToMove =
    let
        newPosition =
            List.map (moveBlockHorizontally blocksToMove) model.carPosition
    in
    if isInsideBoard newPosition then
        { model | carPosition = newPosition }

    else
        model



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , if model.gameOver then
            Sub.none

          else
            Sub.batch
                [ Time.every (1000 * secondsBetwenLevels) NextLevel
                , Time.every (toFloat (100 - model.level * 10)) Tick

                -- , Browser.Events.onAnimationFrameDelta (\delta -> AnimationTick delta)
                ]
        ]


type Key
    = Left
    | Right
    | Space
    | Other


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
    toDirection string
        |> KeyPressed


toDirection : String -> Key
toDirection string =
    let
        _ =
            Debug.log "Key" string
    in
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        " " ->
            Space

        _ ->
            Other



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (String.fromInt (getPixelSize model.board.width)), height (String.fromInt (getPixelSize model.board.height)) ]
            ([ drawBoardBorder model ]
                ++ drawBlocks model.carConfig model.carPosition
                ++ drawObstacles model
            )
        , drawGameOver model
        , h5 [] [ text ("Points: " ++ String.fromInt model.points) ]
        , h5 [] [ text ("Level: " ++ String.fromInt model.level) ]
        , span [] [ text "Use space to restart the game." ]
        ]


drawObstacles : Model -> List (Html Msg)
drawObstacles model =
    flatten2D model.obstacles
        |> drawBlocks model.obstacleConfig


drawBlocks : BlockConfig -> List BlockPosition -> List (Html Msg)
drawBlocks config blocks =
    List.map (\pos -> ( config, pos )) blocks
        |> List.map drawBlock


drawBlock : ( BlockConfig, BlockPosition ) -> Html Msg
drawBlock ( config, position ) =
    let
        blockX =
            position.x * blockSize

        blockY =
            position.y * blockSize
    in
    rect
        [ width (String.fromInt config.width)
        , height (String.fromInt config.height)
        , position.x * blockSize |> String.fromInt |> x
        , position.y * blockSize |> String.fromInt |> y
        , fill config.color
        , stroke config.border
        , strokeWidth "1"
        ]
        []


drawBoardBorder : Model -> Html Msg
drawBoardBorder model =
    rect
        [ getPixelSize model.board.width |> String.fromInt |> width
        , getPixelSize model.board.height |> String.fromInt |> height
        , fill "none"
        , stroke model.board.border
        , strokeWidth "1"
        ]
        []


drawGameOver : Model -> Html Msg
drawGameOver model =
    if model.gameOver then
        h1 [ Html.Attributes.class "game-over" ] [ Html.text "GAME OVER" ]

    else
        Html.text ""



---- UTILS -----


flatten2D : List (List a) -> List a
flatten2D list =
    List.foldr (++) [] list
