module Frontend exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ionicon
import Ionicon.Android as Android
import Lamdera exposing (sendToBackend)
import List.Extra exposing (setAt)
import String exposing (fromInt)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> NoOpFront
        , onUrlChange = \_ -> NoOpFront
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


initModel : FrontendModel
initModel =
    { clientId = ""
    , errors = []

    -- Player stuff
    , gameId = ""
    , playerName = ""
    , truths = [ Answer "" Truth, Answer "" Truth, Answer "" Truth ]
    , lie = Answer "" Lie
    , currentGuess = Nothing

    -- The Game
    , game = Nothing
    , allPlayers = Dict.empty
    , correctAnswerForRound = ""
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initModel, sendToBackend ClientJoin )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFront ->
            ( model, Cmd.none )

        UpdatedGameId gameId ->
            ( { model | gameId = gameId }, Cmd.none )

        UpdatedName name ->
            ( { model | playerName = name }, Cmd.none )

        UpdatedTruth index text ->
            ( { model
                | truths =
                    model.truths
                        |> setAt index (Answer text Truth)
              }
            , Cmd.none
            )

        UpdatedLie text ->
            ( { model | lie = Answer text Lie }, Cmd.none )

        HostGame ->
            ( model, sendToBackend (CreateGame model.gameId model.playerName) )

        JoinGame ->
            ( model, sendToBackend (AddPlayerToGame model.gameId model.playerName) )

        SubmitAnswers ->
            -- TODO:  Validate the inputs!!
            ( model, sendToBackend (LockPlayerAnswers model.gameId model.playerName model.truths model.lie) )

        StartGame ->
            -- onClick event for the host-only "Start Game" button
            ( model, sendToBackend (StartTheGame model.gameId) )

        UpdatedPlayerGuess index ->
            case model.currentGuess of
                Just curIdx ->
                    if curIdx == index then
                        ( { model | currentGuess = Nothing }, Cmd.none )

                    else
                        ( { model | currentGuess = Just index }, Cmd.none )

                Nothing ->
                    ( { model | currentGuess = Just index, errors = [] }, Cmd.none )

        SubmitGuess ->
            case model.currentGuess of
                Just guess ->
                    ( { model | currentGuess = Nothing }, sendToBackend (SubmitPlayerGuess model.gameId model.playerName guess) )

                Nothing ->
                    ( { model | errors = [ NoGuessChosen ] }, Cmd.none )

        GoToNextRound ->
            ( model, sendToBackend (NextRoundForGame model.gameId) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Error errorList ->
            -- Something bad happened
            ( { model | errors = errorList }, Cmd.none )

        GameCreated game allPlayers ->
            -- The host's "new" game is ready for them
            ( { model
                | game = Just game
                , allPlayers = allPlayers
                , errors = []
              }
            , Cmd.none
            )

        PlayerJoinedTF game allPlayers ->
            -- The new player has joined an existing game
            ( { model
                | game = Just game
                , allPlayers = allPlayers
                , errors = []
              }
            , Cmd.none
            )

        PlayerUpdated player ->
            -- One of the players in the game was updated (usually this is their state)
            ( { model
                | allPlayers = model.allPlayers |> Dict.insert player.id_ player
                , errors = []
              }
            , Cmd.none
            )

        LastPlayerLocked player game ->
            -- The final player in the game has locked their answers in
            ( { model
                | game = Just game
                , allPlayers = model.allPlayers |> Dict.insert player.id_ player
                , errors = []
              }
            , Cmd.none
            )

        NextRoundReady game players ->
            -- The next player to be guessed on has been chosen and set
            ( { model
                | game = Just game
                , allPlayers = players
                , errors = []
              }
            , Cmd.none
            )

        PlayerGuessed game player ->
            ( { model
                | game = Just game
                , allPlayers = model.allPlayers |> Dict.insert player.id_ player
                , errors = []
              }
            , Cmd.none
            )

        AnswerWithUpdatedScores game players correctAnswer ->
            ( { model
                | game = Just game
                , allPlayers = players
                , correctAnswerForRound = correctAnswer
              }
            , Cmd.none
            )

        GameOver game players ->
            ( { model
                | game = Just game
                , allPlayers = players
              }
            , Cmd.none
            )


isHostOf : Game -> PlayerId -> Bool
isHostOf game playerId =
    playerId == game.hostPlayer


renderErrorSection : Model -> Html FrontendMsg
renderErrorSection model =
    div [ class "errorList" ]
        (model.errors
            |> List.map (\e -> p [ class "error" ] [ text (errorToString e) ])
        )


renderHomePage : FrontendModel -> List (Html FrontendMsg)
renderHomePage model =
    [ div
        [ class "container" ]
        [ h1 [] [ text "3 Truths 1 Lie" ]
        , table [ class "gameForm", align "center" ]
            [ tr [ class "formField", align "right" ]
                [ td [] [ label [ for "gameId" ] [ text "Game ID:" ] ]
                , td []
                    [ input
                        [ Html.Attributes.id "gameId"
                        , type_ "text"
                        , class "formInput"
                        , onInput UpdatedGameId
                        ]
                        []
                    ]
                ]
            , tr [ class "formField", align "right" ]
                [ td [] [ label [ for "playerName" ] [ text "Player Name:" ] ]
                , td []
                    [ input
                        [ Html.Attributes.id "playerName"
                        , type_ "text"
                        , class "formInput"
                        , onInput UpdatedName
                        ]
                        []
                    ]
                ]
            ]
        , button
            [ class "largeButton"
            , onClick HostGame
            ]
            [ text "Host a Game" ]
        , button
            [ class "largeButton"
            , onClick JoinGame
            ]
            [ text "Join a Game" ]
        , renderErrorSection model
        ]
    ]


renderGameTemplate : FrontendModel -> List (Html FrontendMsg) -> List (Html FrontendMsg)
renderGameTemplate model content =
    [ div [ class "playerContainer" ]
        [ h3 [] [ text "Players" ]
        , ul [ class "playerList" ]
            (model.allPlayers
                |> Dict.map
                    (\name player ->
                        let
                            ( icon, iconColor ) =
                                case player.status of
                                    NotReady ->
                                        ( Android.close, Types.redIcon )

                                    Guessing ->
                                        ( Android.moreHorizontal, Types.redIcon )

                                    _ ->
                                        ( Android.checkmarkCircle, Types.greenIcon )
                        in
                        li [ class "playerCard" ]
                            [ div [ class "playerDiv" ]
                                [ icon 32 iconColor
                                , p [ class "playerName" ]
                                    [ text
                                        (name
                                            ++ " - "
                                            ++ String.fromInt player.score
                                            ++ " points "
                                        )
                                    ]
                                ]
                            ]
                    )
                |> Dict.values
            )
        ]
    , div
        [ class "container" ]
        (content
            |> List.append
                [ h1 [] [ text "3 Truths 1 Lie" ]
                ]
        )
    ]


renderGamePage : GameState -> FrontendModel -> List (Html FrontendMsg)
renderGamePage gameState model =
    let
        ( formHidden, waitingHidden ) =
            case model.allPlayers |> Dict.get model.playerName of
                Just player ->
                    if player.status == Ready then
                        ( " hidden", "" )

                    else
                        ( "", " hidden" )

                Nothing ->
                    ( "", " hidden" )

        ( startButtonHidden, waitingText ) =
            case model.game of
                Just game ->
                    case ( model.playerName |> isHostOf game, gameState ) of
                        ( True, AllReady ) ->
                            ( "", "Everyone is ready!" )

                        ( False, AllReady ) ->
                            ( " hidden", "Waiting for host to start the game..." )

                        ( _, _ ) ->
                            ( " hidden", "Waiting for other players..." )

                Nothing ->
                    ( " hidden", "Waiting for other players..." )
    in
    renderGameTemplate model
        [ h3 [ class formHidden ] [ text "Please submit your Truths and Lie:" ]
        , table
            [ class ("answerForm" ++ formHidden), align "center" ]
            ([ tr [ align "right" ]
                [ td []
                    [ label
                        [ class "answerLabel"
                        , for "lie"
                        ]
                        [ text "Lie:" ]
                    ]
                , td []
                    [ input
                        [ class "answerInput"
                        , Html.Attributes.id "lie"
                        , onInput UpdatedLie
                        ]
                        []
                    ]
                ]
             ]
                |> List.append
                    ([ 1, 2, 3 ]
                        |> List.map
                            (\idx ->
                                [ tr [ align "right" ]
                                    [ td []
                                        [ label
                                            [ class "answerLabel", for ("truth" ++ fromInt idx) ]
                                            [ text ("Truth #" ++ fromInt idx ++ ":") ]
                                        ]
                                    , td []
                                        [ input
                                            [ class "answerInput"
                                            , Html.Attributes.id ("truth" ++ fromInt idx)
                                            , onInput (UpdatedTruth (idx - 1))
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            )
                        |> List.concat
                    )
            )
        , button
            [ class ("largeButton" ++ formHidden), onClick SubmitAnswers ]
            [ text "Submit Answers" ]
        , h3 [ class waitingHidden ] [ text waitingText ]
        , renderErrorSection model
        , div [ class ("startButtonContainer" ++ startButtonHidden) ]
            [ button [ class "largeButton", onClick StartGame ] [ text "Start Game" ] ]
        ]


renderGuessPage : FrontendModel -> List (Html FrontendMsg)
renderGuessPage model =
    let
        ( hideClass, waitClass ) =
            case model.allPlayers |> Dict.get model.playerName of
                Just player ->
                    if player.status == Guessed then
                        ( " hidden", "" )

                    else
                        ( "", " hidden" )

                Nothing ->
                    ( "", " hidden" )
    in
    case model.game of
        Just game ->
            case ( game.activePlayer, game.currentAnswers ) of
                ( Just activePlayer, activeAnswers ) ->
                    renderGameTemplate model
                        [ h3 [] [ text ("Guessing for: " ++ activePlayer.id_) ]
                        , div [ class "activeAnswers" ]
                            (activeAnswers
                                |> List.indexedMap
                                    (\i answer ->
                                        let
                                            toggleClass =
                                                case model.currentGuess of
                                                    Just int ->
                                                        if i == int then
                                                            " chosen"

                                                        else
                                                            ""

                                                    Nothing ->
                                                        ""
                                        in
                                        button
                                            [ class ("activeAnswer" ++ toggleClass)
                                            , onClick (UpdatedPlayerGuess i)
                                            ]
                                            [ text answer.text ]
                                    )
                            )
                        , button
                            [ class ("largeButton" ++ hideClass)
                            , onClick SubmitGuess
                            ]
                            [ text "Submit Guess" ]
                        , h3 [ class waitClass ] [ text "Waiting for other players..." ]
                        ]

                ( Nothing, _ ) ->
                    renderGameTemplate model
                        [ h1 [] [ text "FATAL ERROR, NO ACTIVE PLAYER" ] ]

        Nothing ->
            renderGameTemplate model
                [ h1 [] [ text "FATAL ERROR, NO GAME" ] ]


renderResultsPage : FrontendModel -> List (Html FrontendMsg)
renderResultsPage model =
    case model.game of
        Just game ->
            let
                ( hideClass, waitClass, waitText ) =
                    if model.playerName |> isHostOf game then
                        ( "", " hidden", "" )

                    else
                        ( " hidden", "", "Waiting for host to continue..." )
            in
            case game.activePlayer of
                Just activePlayer ->
                    renderGameTemplate model
                        -- TODO:  Hide button for all but host
                        ([ button
                            [ class ("largeButton" ++ hideClass)
                            , onClick GoToNextRound
                            ]
                            [ text "Next Round" ]
                         , h3 [ class waitClass ] [ text waitText ]
                         ]
                            |> List.append
                                (h3 [] [ text (activePlayer.id_ ++ "'s answers:") ]
                                    :: ((activePlayer.lie :: activePlayer.truths)
                                            |> List.map
                                                (\answer ->
                                                    case answer.type_ of
                                                        Truth ->
                                                            [ h3 [ class "truthAnswer" ] [ text "TRUTH:" ]
                                                            , h4 [] [ text answer.text ]
                                                            ]

                                                        Lie ->
                                                            [ h3 [ class "lieAnswer" ] [ text "LIE:" ]
                                                            , h4 [] [ text answer.text ]
                                                            ]
                                                )
                                            |> List.concat
                                       )
                                )
                        )

                Nothing ->
                    renderGameTemplate model
                        [ h1 [] [ text "FATAL ERROR, GAME HAS NO ACTIVE PLAYER" ] ]

        Nothing ->
            renderGameTemplate model
                [ h1 [] [ text "FATAL ERROR, NO GAME" ] ]


renderFinalPage : FrontendModel -> List (Html FrontendMsg)
renderFinalPage model =
    let
        topPlayer =
            model.allPlayers
                |> Dict.values
                |> List.map (\p -> ( p.id_, p.score ))
                |> List.sortBy Tuple.second
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( "INVALID PLAYER", -9999 )
    in
    renderGameTemplate model
        [ h1 [ class "orangeText" ] [ text "GAME OVER!" ]
        , h3 [] [ text "The Winner is..." ]
        , h3 [ class "greenText" ] [ text (Tuple.first topPlayer) ]
        , h3 [] [ text " with " ]
        , h3 [ class "greenText" ] [ text ((topPlayer |> Tuple.second |> String.fromInt) ++ " point(s)!") ]
        ]


view : FrontendModel -> Document FrontendMsg
view model =
    let
        pageContent =
            case model.game of
                Just game ->
                    case game.state of
                        Initial ->
                            renderHomePage model

                        Asking ->
                            renderGamePage Asking model

                        AllReady ->
                            renderGamePage AllReady model

                        PlayersGuessing ->
                            renderGuessPage model

                        Results ->
                            renderResultsPage model

                        FinishedGame ->
                            renderFinalPage model

                Nothing ->
                    renderHomePage model
    in
    { title = "3 Truths 1 Lie"
    , body =
        [ Html.node "link" [ rel "stylesheet", href "/css/main.css" ] []
        , div [ class "flexCenter" ] pageContent
        ]
    }
