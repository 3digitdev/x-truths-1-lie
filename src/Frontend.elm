module Frontend exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (..)
import Dict.Extra exposing (groupBy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ionicon
import Ionicon.Android as Android
import Lamdera exposing (sendToBackend)
import List.Extra exposing (getAt, setAt)
import Set exposing (..)
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
    , guessText = Nothing

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
            ( model, sendToBackend (LockPlayerAnswers model.gameId model.playerName model.truths model.lie) )

        StartGame ->
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
                    let
                        guessText =
                            case model.game of
                                Just game ->
                                    game.currentAnswers
                                        |> List.map .text
                                        |> getAt guess
                                        |> Maybe.withDefault ""

                                Nothing ->
                                    ""
                    in
                    ( { model | currentGuess = Nothing, guessText = Just guessText }, sendToBackend (SubmitPlayerGuess model.gameId model.playerName guess) )

                Nothing ->
                    ( { model | errors = [ NoGuessChosen ] }, Cmd.none )

        GoToNextRound ->
            ( model, sendToBackend (NextRoundForGame model.gameId) )

        BootPlayer playerId ->
            ( model, sendToBackend (DeletePlayer model.gameId playerId) )

        HostDeletesGame ->
            case model.game of
                Just game ->
                    ( model, sendToBackend (DeleteGame game) )

                Nothing ->
                    ( model, Cmd.none )


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
                , currentGuess = Nothing
                , guessText = Nothing
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

        PlayerDeleted game players playerId ->
            if playerId == model.playerName then
                ( { initModel | errors = [ Fatal "You were booted from the game by the host" ] }, Cmd.none )

            else
                ( { model | game = Just game, allPlayers = players }, Cmd.none )

        GameDeleted ->
            ( { initModel | errors = [ Fatal "The game was deleted by the host" ] }, Cmd.none )


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
                        , maxlength 10
                        ]
                        []
                    ]
                , td [] [ text "(max 10 chars)" ]
                ]
            , tr [ class "formField", align "right" ]
                [ td [] [ label [ for "playerName" ] [ text "Player Name:" ] ]
                , td []
                    [ input
                        [ Html.Attributes.id "playerName"
                        , type_ "text"
                        , class "formInput"
                        , onInput UpdatedName
                        , maxlength 16
                        ]
                        []
                    ]
                , td [] [ text "(max 16 chars)" ]
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
    let
        ( deleteButtonHidden, nextButtonHidden ) =
            case model.game of
                Just game ->
                    case ( model.playerName |> isHostOf game, game.activePlayer ) of
                        ( True, Just player ) ->
                            if game.players |> List.member player.id_ then
                                ( "", " hidden" )

                            else
                                ( "", "" )

                        ( True, Nothing ) ->
                            ( "", " hidden" )

                        ( False, _ ) ->
                            ( " hidden", " hidden" )

                Nothing ->
                    ( " hidden", " hidden" )
    in
    [ div [ class "playerContainer" ]
        [ p [] [ text ("GameID: " ++ model.gameId) ]
        , h3 [] [ text "Players" ]
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
                                , div [ class "bootPlayer", onClick (BootPlayer player.id_) ] [ Ionicon.eject 32 Types.blackIcon ]
                                ]
                            ]
                    )
                |> Dict.values
            )
        , button
            [ class ("largeButton deleteButton" ++ deleteButtonHidden)
            , onClick HostDeletesGame
            ]
            [ text "DELETE GAME" ]
        , h4 [ class nextButtonHidden ] [ text "Active player was deleted:" ]
        , button
            [ class ("largeButton deleteButton" ++ nextButtonHidden)
            , onClick GoToNextRound
            ]
            [ text "Next Round" ]
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
    case ( model.game, model.guessText ) of
        ( Just game, Nothing ) ->
            case ( game.activePlayer, game.currentAnswers ) of
                ( Just activePlayer, activeAnswers ) ->
                    let
                        ( hideForm, hideText ) =
                            if activePlayer.id_ == model.playerName then
                                ( "hidden", "" )

                            else
                                ( "", "hidden" )
                    in
                    renderGameTemplate model
                        [ div [ class hideForm ]
                            [ h3 [] [ text ("Guessing for: " ++ activePlayer.id_) ]
                            , p [ class "instructions" ] [ text "Select the lie and hit \"Submit\"! No backsies.  :)" ]
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
                                                , disabled (activePlayer.id_ == model.playerName)
                                                , onClick (UpdatedPlayerGuess i)
                                                ]
                                                [ text answer.text ]
                                        )
                                )
                            , button
                                [ class ("largeButton" ++ hideClass)
                                , disabled (activePlayer.id_ == model.playerName)
                                , onClick SubmitGuess
                                ]
                                [ text "Submit Guess" ]
                            ]
                        , h3 [ class hideText ] [ text "Players are trying to guess your answers!" ]
                        , h3 [ class waitClass ] [ text "Waiting for other players..." ]
                        ]

                ( Nothing, _ ) ->
                    renderGameTemplate model
                        [ h1 [] [ text "FATAL ERROR, NO ACTIVE PLAYER" ] ]

        ( Just game, Just guessText ) ->
            renderGameTemplate model
                [ h3 [] [ text "You guessed:" ]
                , h4 [] [ text guessText ]
                , h3 [ class waitClass ] [ text "Waiting for other players..." ]
                ]

        ( Nothing, _ ) ->
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

                answerMap =
                    game.currentGuesses
                        |> Dict.toList
                        |> groupBy Tuple.second
                        |> Dict.map (\_ tups -> tups |> List.map Tuple.first)
                        |> Dict.map
                            (\index value ->
                                ( game.currentAnswers
                                    |> getAt index
                                    |> Maybe.withDefault (Answer "" Truth)
                                , value
                                )
                            )
                        |> Dict.values

                playerAnswers =
                    game.currentAnswers
                        |> List.indexedMap
                            (\i answer ->
                                answerMap
                                    |> List.filter (\( a, _ ) -> answer.text == a.text)
                                    |> List.head
                                    |> Maybe.withDefault ( answer, [] )
                            )
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
                                    :: (playerAnswers
                                            |> List.map
                                                (\( answer, guessers ) ->
                                                    let
                                                        num =
                                                            guessers |> List.length |> String.fromInt
                                                    in
                                                    case answer.type_ of
                                                        Truth ->
                                                            [ h3
                                                                [ class "truthAnswer"
                                                                , title (guessers |> String.join ", ")
                                                                ]
                                                                [ text ("TRUTH:  (" ++ num ++ ")") ]
                                                            , h4 [] [ text answer.text ]
                                                            ]

                                                        Lie ->
                                                            [ h3
                                                                [ class "lieAnswer"
                                                                , title (guessers |> String.join ", ")
                                                                ]
                                                                [ text ("LIE:  (" ++ num ++ ")") ]
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
        topScore =
            model.allPlayers
                |> Dict.values
                |> List.map (\p -> p.score)
                |> List.sort
                |> List.reverse
                |> List.head
                |> Maybe.withDefault -9999

        topPlayers =
            model.allPlayers
                |> Dict.values
                |> List.filter (\p -> p.score == topScore)
                |> List.map .id_
                |> String.join ", "
    in
    renderGameTemplate model
        [ h1 [ class "orangeText" ] [ text "GAME OVER!" ]
        , h3 [] [ text "The Winner(s)" ]
        , h3 [ class "greenText" ] [ text topPlayers ]
        , h3 [] [ text " with " ]
        , h3 [ class "greenText" ] [ text (String.fromInt topScore ++ " point(s)!") ]
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
