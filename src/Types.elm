module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Set exposing (Set)
import Url exposing (Url)



-- Define your colors


redIcon : RGBA
redIcon =
    RGBA 0.94 0.24 0.3 1


greenIcon : RGBA
greenIcon =
    RGBA 0.17 0.6 0.3 1


blackIcon : RGBA
blackIcon =
    RGBA 0 0 0 1


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias GameId =
    String


type alias Score =
    Int


type AnswerType
    = Truth
    | Lie


type alias Answer =
    { text : String, type_ : AnswerType }


type alias PlayerId =
    String


type alias PlayerMapping =
    Dict PlayerId Player


type Status
    = Ready
    | NotReady
    | Guessing
    | Guessed
    | Finished


type PlayerType
    = Participant
    | Spectator


type alias Player =
    { id_ : PlayerId
    , truths : List Answer
    , lie : Answer
    , score : Score
    , status : Status
    , type_ : PlayerType
    }


type GameState
    = Initial
    | Asking
    | AllReady
    | PlayersGuessing
    | Results
    | FinishedGame


type alias Game =
    { id_ : GameId
    , hostPlayer : PlayerId
    , players : List PlayerId
    , activePlayer : Maybe Player
    , playerTurnOver : List PlayerId
    , currentAnswers : List Answer
    , currentGuesses : Dict PlayerId Int
    , state : GameState
    }


type ErrorType
    = GameRequired
    | GameExists
    | GameNotExist
    | NameRequired
    | NameExists
    | NoGuessChosen
    | AutoSpectator
    | Fatal String


errorToString : ErrorType -> String
errorToString errorType =
    case errorType of
        GameRequired ->
            "You must have a non-empty Game ID"

        GameExists ->
            "A game already exists with that ID"

        GameNotExist ->
            "A game does not exist with that ID"

        NameRequired ->
            "You must have a non-empty Player Name"

        NameExists ->
            "A player already exists with that name"

        NoGuessChosen ->
            "You must select a guess to continue"

        AutoSpectator ->
            "You have joined a game in progress and are a spectator.  You can still guess!"

        Fatal someError ->
            "FATAL:  " ++ someError


type alias FrontendModel =
    { clientId : ClientId
    , errors : List ErrorType

    -- Player stuff
    , gameId : GameId
    , playerName : PlayerId
    , isSpectator : Bool
    , truths : List Answer
    , lie : Answer
    , currentGuess : Maybe Int
    , guessText : Maybe String

    -- The game
    , game : Maybe Game
    , allPlayers : PlayerMapping
    , correctAnswerForRound : String
    }


type alias ClientMap =
    Dict GameId (List ClientId)


type alias BackendModel =
    { clients : Set ClientId
    , clientMap : ClientMap
    , games : Dict GameId Game
    , players : PlayerMapping
    , playerClientMap : Dict PlayerId ClientId
    }


type FrontendMsg
    = UpdatedGameId GameId
    | UpdatedName PlayerId
    | UpdatedTruth Int String
    | UpdatedLie String
    | ToggleSpectator
    | ToggleParticipant
    | UpdatedPlayerGuess Int
    | HostGame
    | JoinGame
    | SubmitAnswers
    | StartGame
    | SubmitGuess
    | GoToNextRound
    | HostDeletesGame
    | BootPlayer PlayerId
    | NoOpFront


type ToBackend
    = ClientJoin
    | CreateGame GameId PlayerId Bool
    | AddPlayerToGame GameId PlayerId Bool
    | LockPlayerAnswers GameId PlayerId (List Answer) Answer
    | StartTheGame GameId
    | SubmitPlayerGuess GameId PlayerId Int
    | NextRoundForGame GameId
    | DeletePlayer GameId PlayerId
    | DeleteGame Game


type BackendMsg
    = PickNextPlayer GameId
    | SetNextPlayer Game ( Maybe PlayerId, List PlayerId )
    | AnswersShuffled Game (List Answer)
    | ScoreAndRevealAnswer Game
    | CleanupGame Game


type ToFrontend
    = Error (List ErrorType)
    | GameCreated Game PlayerMapping
    | PlayerJoinedTF Game PlayerMapping Bool
    | PlayerUpdated Player
    | LastPlayerLocked Player Game
    | NextRoundReady Game PlayerMapping
    | PlayerGuessed Game Player
    | AnswerWithUpdatedScores Game PlayerMapping String
    | GameOver Game PlayerMapping
    | PlayerDeleted Game PlayerMapping PlayerId
    | GameDeleted
