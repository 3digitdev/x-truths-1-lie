module Backend exposing (..)

import Dict exposing (Dict, get, insert, member)
import Html
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import List.Extra exposing (setAt)
import Maybe.Extra exposing (values)
import Random exposing (generate)
import Random.List as RL
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { clients = Set.empty
      , clientMap = Dict.empty
      , games = Dict.empty
      , players = Dict.empty
      , playerClientMap = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        PickNextPlayer gameId ->
            -- Gather players that haven't had a turn, and pick one to be "next"
            case model.games |> Dict.get gameId of
                Just game ->
                    let
                        remainingPlayerIds =
                            model.players
                                |> Dict.keys
                                |> List.filter (\pid -> game.players |> List.member pid)
                                |> List.filter (\pid -> not (game.playerTurnOver |> List.member pid))
                    in
                    ( model
                    , Random.generate (SetNextPlayer game) (RL.choose remainingPlayerIds)
                    )

                Nothing ->
                    Fatal "Couldn't find the game to pick the player"
                        |> broadcastError model gameId

        SetNextPlayer game ( maybeNextPid, otherPids ) ->
            -- We picked the next player, lets set them as active, and then shuffle their answers
            case maybeNextPid of
                Just nextPid ->
                    case model.players |> Dict.get nextPid of
                        Just nextPlayer ->
                            let
                                newPlayer =
                                    { nextPlayer | status = Guessed }

                                newGame =
                                    { game
                                        | activePlayer = Just newPlayer
                                        , playerTurnOver = nextPid :: game.playerTurnOver
                                    }

                                allAnswers =
                                    newPlayer.lie :: newPlayer.truths
                            in
                            ( { model
                                | games = model.games |> Dict.insert newGame.id_ newGame
                                , players = model.players |> Dict.insert newPlayer.id_ newPlayer
                              }
                            , Random.generate (AnswersShuffled newGame) (RL.shuffle allAnswers)
                            )

                        Nothing ->
                            -- Something went horribly wrong
                            Fatal ("Couln't find next player [" ++ nextPid ++ "]")
                                |> broadcastError model game.id_

                Nothing ->
                    let
                        newGame =
                            { game
                                | state = FinishedGame
                                , activePlayer = Nothing
                                , currentAnswers = []
                                , currentGuesses = Dict.empty
                            }

                        newModel =
                            { model
                                | games = model.games |> Dict.insert game.id_ newGame
                            }
                    in
                    update (CleanupGame newGame) newModel

        AnswersShuffled game shuffledAnswers ->
            -- New active player's answers are now shuffled, tell the players about the new round
            let
                newGame =
                    { game | currentAnswers = shuffledAnswers }
            in
            ( { model
                | games =
                    model.games
                        |> Dict.insert game.id_ newGame
              }
            , NextRoundReady newGame (newGame |> getPlayerMap model)
                |> broadcast model.clientMap newGame.id_
            )

        ScoreAndRevealAnswer game ->
            -- All players have guessed, time to update scores and reveal the Lie
            let
                correctIndex =
                    game.currentAnswers
                        |> List.map .type_
                        |> List.Extra.elemIndex Lie
                        |> Maybe.withDefault -1

                correctAnswer =
                    case game.currentAnswers |> List.Extra.getAt correctIndex of
                        Just answer ->
                            answer.text

                        Nothing ->
                            "FATAL ERROR GETTING ANSWER #" ++ String.fromInt correctIndex

                -- Map PlayerIds to the increase (or not) to their score
                scoreMods =
                    game.currentGuesses
                        |> Dict.toList
                        |> List.map (\guessTuple -> getScoreMod guessTuple correctIndex)
                        |> Dict.fromList

                -- Update players in the game with the new score modifier they have
                -- If the player isn't in the game, then skip them
                newPlayers =
                    game.players
                        |> List.map (\pid -> model.players |> Dict.get pid)
                        |> Maybe.Extra.values
                        |> List.map
                            (\player ->
                                case scoreMods |> Dict.get player.id_ of
                                    Just scoreMod ->
                                        ( player.id_, { player | score = player.score + scoreMod } )

                                    Nothing ->
                                        ( player.id_, player )
                            )
                        |> Dict.fromList

                -- Get the update version of all the players for the game to send to frontend
                gamePlayers =
                    newPlayers
                        |> Dict.filter (\k _ -> game.players |> List.member k)
            in
            ( { model | players = newPlayers }
            , AnswerWithUpdatedScores game gamePlayers correctAnswer
                |> broadcast model.clientMap game.id_
            )

        CleanupGame game ->
            -- Delete references to the game, players, and clients, then return the game
            ( { model
                | clientMap = model.clientMap |> Dict.remove game.id_
                , players =
                    model.players
                        |> Dict.filter
                            (\k _ -> game.players |> List.member k |> not)
                , games = model.games |> Dict.remove game.id_
              }
            , GameOver game (game |> getPlayerMap model)
                |> broadcast model.clientMap game.id_
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientJoin ->
            -- New client connected to the backend, nothing done yet.  Tell the client their ID
            -- TODO:  Does the frontend need to know their ID?  This might just be a NoOp.
            ( { model | clients = model.clients |> Set.insert clientId }
            , Cmd.none
            )

        CreateGame gameId hostName isSpectator ->
            -- A new host asked to create a game
            case model |> validateInput gameId hostName Host of
                ( [], _ ) ->
                    let
                        hostPlayer =
                            { id_ = hostName
                            , truths = []
                            , lie = Answer "" Lie
                            , score = 0
                            , status =
                                if isSpectator then
                                    Ready

                                else
                                    NotReady
                            , type_ =
                                if isSpectator then
                                    Spectator

                                else
                                    Participant
                            }

                        newGame =
                            { id_ = gameId
                            , hostPlayer = hostName
                            , players = [ hostName ]
                            , activePlayer = Nothing
                            , playerTurnOver = []
                            , currentAnswers = []
                            , currentGuesses = Dict.empty
                            , state = Asking
                            }

                        newClientMapList =
                            model.clientMap
                                |> Dict.get gameId
                                |> Maybe.withDefault []
                                |> List.append [ clientId ]

                        newPCMap =
                            model.playerClientMap |> Dict.insert hostName clientId

                        allPlayers =
                            newGame |> getPlayerMapIncluding hostName hostPlayer model
                    in
                    ( { model
                        | clientMap = model.clientMap |> Dict.insert gameId newClientMapList
                        , games = model.games |> Dict.insert gameId newGame
                        , players = model.players |> Dict.insert hostName hostPlayer
                        , playerClientMap = newPCMap
                      }
                    , sendToFrontend clientId (GameCreated newGame allPlayers)
                    )

                ( errors, _ ) ->
                    ( model, sendToFrontend clientId (Error errors) )

        AddPlayerToGame gameId playerName isSpectator ->
            -- A new player wants to join an existing game
            case model |> validateInput gameId playerName Join of
                ( [], Just game ) ->
                    let
                        gameInProgress =
                            [ PlayersGuessing, Results, FinishedGame ]
                                |> List.member game.state

                        newPlayer =
                            { id_ = playerName
                            , truths = []
                            , lie = Answer "" Lie
                            , score = 0
                            , status =
                                if isSpectator then
                                    Ready

                                else
                                    NotReady
                            , type_ =
                                if isSpectator || gameInProgress then
                                    Spectator

                                else
                                    Participant
                            }

                        newTurnOver =
                            if gameInProgress then
                                playerName :: game.playerTurnOver

                            else
                                game.playerTurnOver

                        existingGame =
                            { game
                                | players = playerName :: game.players
                                , playerTurnOver = newTurnOver
                            }

                        newClientMap =
                            model.clientMap |> updateClientMap gameId clientId

                        newPCMap =
                            model.playerClientMap |> Dict.insert playerName clientId

                        newPlayers =
                            existingGame |> getPlayerMapIncluding playerName newPlayer model
                    in
                    ( { model
                        | clientMap = newClientMap
                        , games = model.games |> Dict.insert game.id_ existingGame
                        , players =
                            model.players
                                |> Dict.insert playerName newPlayer
                        , playerClientMap = newPCMap
                      }
                    , PlayerJoinedTF existingGame newPlayers gameInProgress |> broadcast newClientMap gameId
                    )

                ( errors, _ ) ->
                    ( model, sendToFrontend clientId (Error errors) )

        LockPlayerAnswers gameId playerId truths lie ->
            -- A player submitted their answers
            case ( Dict.get gameId model.games, Dict.get playerId model.players ) of
                ( Just game, Just player ) ->
                    let
                        newPlayer =
                            { player | truths = truths, lie = lie, status = Ready }

                        subModel =
                            { model | players = model.players |> Dict.insert playerId newPlayer }

                        newGame =
                            if game |> allPlayersAre Ready subModel then
                                { game | state = AllReady }

                            else
                                game

                        sendMsg =
                            case newGame.state of
                                AllReady ->
                                    LastPlayerLocked newPlayer newGame |> broadcast model.clientMap gameId

                                _ ->
                                    PlayerUpdated newPlayer |> broadcast model.clientMap gameId
                    in
                    ( { subModel | games = model.games |> Dict.insert gameId newGame }, sendMsg )

                ( _, _ ) ->
                    Fatal ("Couldn't find player [" ++ playerId ++ "]")
                        |> broadcastError model gameId

        StartTheGame gameId ->
            -- The host clicked the button to start the game
            case model.games |> Dict.get gameId of
                Just game ->
                    let
                        newPlayers =
                            game.players
                                |> List.map (\pid -> model.players |> Dict.get pid)
                                |> values
                                |> List.map (\p -> { p | status = Guessing })
                                |> List.map (\p -> ( p.id_, p ))
                                |> Dict.fromList

                        spectators =
                            game.players
                                |> List.filter (\pid -> model |> playerIsSpectator pid)

                        newGame =
                            { game | state = PlayersGuessing, playerTurnOver = spectators }
                    in
                    update (PickNextPlayer gameId)
                        { model
                            | games = model.games |> Dict.insert gameId newGame
                            , players = model.players |> Dict.union newPlayers
                        }

                Nothing ->
                    Fatal ("Game [" ++ gameId ++ "] not found")
                        |> broadcastError model gameId

        SubmitPlayerGuess gameId playerId guess ->
            case ( model.games |> Dict.get gameId, model.players |> Dict.get playerId ) of
                ( Just game, Just player ) ->
                    let
                        newPlayer =
                            { player | status = Guessed }

                        subModel =
                            { model | players = model.players |> Dict.insert playerId newPlayer }

                        gamePlayers =
                            game.players
                                |> List.map (\pid -> subModel.players |> Dict.get pid)
                                |> Maybe.Extra.values
                                |> List.map .status

                        newGame =
                            if game |> allPlayersAre Guessed subModel then
                                { game
                                    | state = Results
                                    , currentGuesses =
                                        game.currentGuesses
                                            |> Dict.insert playerId guess
                                }

                            else
                                { game
                                    | currentGuesses =
                                        game.currentGuesses
                                            |> Dict.insert playerId guess
                                }

                        finalModel =
                            { subModel | games = model.games |> Dict.insert gameId newGame }
                    in
                    case newGame.state of
                        Results ->
                            update (ScoreAndRevealAnswer newGame) finalModel

                        _ ->
                            ( finalModel
                            , PlayerGuessed newGame newPlayer
                                |> broadcast model.clientMap gameId
                            )

                _ ->
                    Fatal "Couldn't find game or player when submitting guess"
                        |> broadcastError model gameId

        NextRoundForGame gameId ->
            case model.games |> Dict.get gameId of
                Just game ->
                    let
                        newGame =
                            { game
                                | state = PlayersGuessing
                                , currentAnswers = []
                                , currentGuesses = Dict.empty
                                , activePlayer = Nothing
                            }

                        newPlayers =
                            model.players
                                |> Dict.union
                                    (game
                                        |> getPlayerMap model
                                        |> Dict.map
                                            (\_ v ->
                                                { v | status = Guessing }
                                            )
                                    )

                        newModel =
                            { model
                                | games = model.games |> Dict.insert gameId newGame
                                , players = newPlayers
                            }
                    in
                    update (PickNextPlayer gameId) newModel

                Nothing ->
                    Fatal "Couldn't find game to go to next round"
                        |> broadcastError model gameId

        DeletePlayer gameId playerId ->
            let
                ( newGame, players ) =
                    case model.games |> Dict.get gameId of
                        Just game ->
                            ( Just
                                { game
                                    | players = game.players |> List.filter ((/=) playerId)
                                    , currentGuesses = game.currentGuesses |> Dict.remove playerId
                                    , playerTurnOver = game.playerTurnOver |> List.filter ((/=) playerId)
                                }
                            , game.players
                                |> List.filter ((/=) playerId)
                                |> List.map
                                    (\pid -> model.players |> Dict.get pid)
                                |> values
                                |> List.map (\p -> ( p.id_, p ))
                                |> Dict.fromList
                            )

                        Nothing ->
                            ( Nothing, Dict.empty )

                playerClientId =
                    model.playerClientMap
                        |> Dict.get playerId
                        |> Maybe.withDefault ""
            in
            case newGame of
                Just ng ->
                    let
                        newClientMap =
                            model.clientMap
                                |> Dict.insert ng.id_
                                    (model.clientMap
                                        |> Dict.get ng.id_
                                        |> Maybe.withDefault []
                                        |> List.filter ((/=) playerClientId)
                                    )
                    in
                    ( { model
                        | players = model.players |> Dict.remove playerId
                        , games = model.games |> Dict.insert ng.id_ ng
                        , clientMap = newClientMap
                        , playerClientMap = model.playerClientMap |> Dict.remove playerId
                      }
                    , PlayerDeleted ng players playerId |> broadcast model.clientMap ng.id_
                    )

                Nothing ->
                    Fatal "Couldn't find game when booting player"
                        |> broadcastError model gameId

        DeleteGame game ->
            let
                newPCMap =
                    model.playerClientMap
                        |> Dict.filter (\k v -> game.players |> List.member k |> not)
            in
            ( { model
                | clientMap = model.clientMap |> Dict.remove game.id_
                , players =
                    model.players
                        |> Dict.filter (\k _ -> game.players |> List.member k |> not)
                , games = model.games |> Dict.remove game.id_
                , playerClientMap = newPCMap
              }
            , GameDeleted |> broadcast model.clientMap game.id_
            )


playerIsSpectator : PlayerId -> Model -> Bool
playerIsSpectator pid model =
    case model.players |> Dict.get pid of
        Just player ->
            player.type_ == Spectator

        Nothing ->
            False


getScoreMod : ( PlayerId, Int ) -> Int -> ( PlayerId, Int )
getScoreMod ( playerId, guess ) correctIndex =
    if guess == correctIndex then
        ( playerId, 1 )

    else
        ( playerId, 0 )


type ValidationSource
    = Host
    | Join


validateInput : GameId -> PlayerId -> ValidationSource -> Model -> ( List ErrorType, Maybe Game )
validateInput gameId playerId source model =
    let
        ( gameError, game ) =
            if gameId == "" then
                ( Just GameRequired, Nothing )

            else
                case ( source, Dict.member gameId model.games ) of
                    ( Host, True ) ->
                        ( Just GameExists, Nothing )

                    ( Join, False ) ->
                        ( Just GameNotExist, Nothing )

                    _ ->
                        ( Nothing, Dict.get gameId model.games )

        nameError =
            if playerId == "" then
                Just NameRequired

            else if Dict.member playerId model.players then
                Just NameExists

            else
                Nothing

        errors =
            case ( gameError, nameError ) of
                ( Nothing, Nothing ) ->
                    []

                ( Just a, Nothing ) ->
                    [ a ]

                ( Nothing, Just b ) ->
                    [ b ]

                ( Just a, Just b ) ->
                    [ a, b ]
    in
    ( errors, game )


getPlayerMap : BackendModel -> Game -> PlayerMapping
getPlayerMap model game =
    model.players
        |> Dict.filter (\k _ -> game.players |> List.member k)


getPlayerMapIncluding : PlayerId -> Player -> BackendModel -> Game -> PlayerMapping
getPlayerMapIncluding playerId player model game =
    game
        |> getPlayerMap model
        |> Dict.insert playerId player


updateClientMap : GameId -> ClientId -> ClientMap -> ClientMap
updateClientMap gameId clientId clientMap =
    clientMap
        |> Dict.insert gameId
            (clientMap
                |> Dict.get gameId
                |> Maybe.withDefault []
                |> List.append [ clientId ]
            )


broadcast : ClientMap -> GameId -> ToFrontend -> Cmd BackendMsg
broadcast clientMap gameId msg =
    case Dict.get gameId clientMap of
        Nothing ->
            Cmd.none

        Just clientList ->
            clientList
                |> List.map (\c -> sendToFrontend c msg)
                |> Cmd.batch


broadcastError : BackendModel -> GameId -> ErrorType -> ( BackendModel, Cmd BackendMsg )
broadcastError model gameId err =
    ( model, Error [ err ] |> broadcast model.clientMap gameId )


allPlayersAre : Status -> BackendModel -> Game -> Bool
allPlayersAre status model game =
    game.players
        |> List.map (\p -> model.players |> Dict.get p)
        |> values
        |> List.all (\p -> p.status == status)
