module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (empty)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
        , view =
            \model ->
                { title = "3 Truths 1 Lie"
                , body = [ view model ]
                }
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


toString : Status -> String
toString status =
    case status of
        Ready ->
            "[Ready]"

        NotReady ->
            "[Not Ready]"

        Guessed ->
            "[Guessed]"

        Guessing ->
            "[Guessing]"

        Finished ->
            "[Finished]"


renderErrorSection : Model -> Html FrontendMsg
renderErrorSection model =
    div [ class "errorList" ]
        (model.errors
            |> List.map (\e -> p [ class "error" ] [ text (errorToString e) ])
        )


renderHomePage : FrontendModel -> Html FrontendMsg
renderHomePage model =
    div
        [ class "container" ]
        [ h1 [] [ text "3 Truths 1 Lie" ]
        , label [ for "gameId" ] [ text "Game ID:" ]
        , input
            [ Html.Attributes.id "gameId"
            , onInput UpdatedGameId
            ]
            []
        , br [] []
        , label [ for "playerName" ] [ text "playerName:" ]
        , input
            [ Html.Attributes.id "playerName"
            , onInput UpdatedName
            ]
            []
        , br [] []
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


renderGameTemplate : FrontendModel -> List (Html FrontendMsg) -> Html FrontendMsg
renderGameTemplate model content =
    div
        []
        (content
            |> List.append
                [ h1 [] [ text "3 Truths 1 Lie" ]
                , h2 [] [ text model.gameId ]
                , h3 [] [ text "Players:" ]
                , ul [ class "playerList" ]
                    (model.allPlayers
                        |> Dict.map
                            (\name player ->
                                li [ class "playerCard" ]
                                    [ text
                                        (name
                                            ++ ": "
                                            ++ String.fromInt player.score
                                            ++ " points "
                                            ++ toString player.status
                                        )
                                    ]
                            )
                        |> Dict.values
                    )
                ]
        )


renderGamePage : GameState -> FrontendModel -> Html FrontendMsg
renderGamePage gameState model =
    let
        formHidden =
            case model.allPlayers |> Dict.get model.playerName of
                Just player ->
                    if player.status == Ready then
                        " hideForm"

                    else
                        ""

                Nothing ->
                    ""

        startButtonHidden =
            case model.game of
                Just game ->
                    case ( game.hostPlayer == model.playerName, gameState ) of
                        ( True, AllReady ) ->
                            ""

                        ( _, _ ) ->
                            " hideStartButton"

                Nothing ->
                    " hideStartButton"
    in
    renderGameTemplate model
        [ h3 [] [ text "Please submit your Truths and Lie:" ]
        , div
            [ class ("answerForm" ++ formHidden) ]
            ([ label [ for "lie" ] [ text "Lie:" ]
             , input [ Html.Attributes.id "lie", onInput UpdatedLie ] []
             , br [] []
             , button
                [ class "largeButton", onClick SubmitAnswers ]
                [ text "Submit Answers" ]
             , renderErrorSection model
             ]
                |> List.append
                    ([ 1, 2, 3 ]
                        |> List.map
                            (\idx ->
                                [ label [ for ("truth" ++ fromInt idx) ] [ text ("Truth #" ++ fromInt idx ++ ":") ]
                                , input
                                    [ Html.Attributes.id ("truth" ++ fromInt idx)
                                    , onInput (UpdatedTruth (idx - 1))
                                    ]
                                    []
                                , br [] []
                                ]
                            )
                        |> List.concat
                    )
            )
        , div [ class ("startButtonContainer" ++ startButtonHidden) ]
            [ button [ class "largeButton", onClick StartGame ] [ text "Start Game" ] ]
        ]


renderGuessPage : FrontendModel -> Html FrontendMsg
renderGuessPage model =
    case model.game of
        Just game ->
            case ( game.activePlayer, game.currentAnswers ) of
                ( Just activePlayer, activeAnswers ) ->
                    renderGameTemplate model
                        [ h3 [] [ text ("Guessing for " ++ activePlayer.id_) ]
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
                        , button [ class "submitGuess", onClick SubmitGuess ] [ text "Submit Guess" ]
                        ]

                ( Nothing, _ ) ->
                    renderGameTemplate model
                        [ h1 [] [ text "FATAL ERROR, NO ACTIVE PLAYER" ] ]

        Nothing ->
            renderGameTemplate model
                [ h1 [] [ text "FATAL ERROR, NO GAME" ] ]


renderResultsPage : FrontendModel -> Html FrontendMsg
renderResultsPage model =
    case model.game of
        Just game ->
            case game.activePlayer of
                Just activePlayer ->
                    renderGameTemplate model
                        -- TODO:  Hide button for all but host
                        ([ button
                            [ class "largeButton"
                            , onClick GoToNextRound
                            ]
                            [ text "Next Round" ]
                         ]
                            |> List.append
                                (h4 [] [ text (activePlayer.id_ ++ "'s answers:") ]
                                    :: ((activePlayer.lie :: activePlayer.truths)
                                            |> List.map
                                                (\answer ->
                                                    case answer.type_ of
                                                        Truth ->
                                                            h3 [ class "truthAnswer" ] [ text ("TRUTH: " ++ answer.text) ]

                                                        Lie ->
                                                            h3 [ class "lieAnswer" ] [ text ("LIE: " ++ answer.text) ]
                                                )
                                       )
                                )
                        )

                Nothing ->
                    renderGameTemplate model
                        [ h1 [] [ text "FATAL ERROR, GAME HAS NO ACTIVE PLAYER" ] ]

        Nothing ->
            renderGameTemplate model
                [ h1 [] [ text "FATAL ERROR, NO GAME" ] ]


renderFinalPage : FrontendModel -> Html FrontendMsg
renderFinalPage model =
    div [] [ text "Final Page" ]


view : FrontendModel -> Html FrontendMsg
view model =
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
