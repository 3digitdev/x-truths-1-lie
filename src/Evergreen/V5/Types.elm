module Evergreen.V5.Types exposing (..)

import Dict
import Lamdera
import Set


type ErrorType
    = GameRequired
    | GameExists
    | GameNotExist
    | NameRequired
    | NameExists
    | NoGuessChosen
    | AutoSpectator
    | Fatal String


type alias GameId = String


type alias PlayerId = String


type AnswerType
    = Truth
    | Lie


type alias Answer = 
    { text : String
    , type_ : AnswerType
    }


type alias Score = Int


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
    , truths : (List Answer)
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
    , players : (List PlayerId)
    , activePlayer : (Maybe Player)
    , playerTurnOver : (List PlayerId)
    , currentAnswers : (List Answer)
    , currentGuesses : (Dict.Dict PlayerId Int)
    , state : GameState
    }


type alias PlayerMapping = (Dict.Dict PlayerId Player)


type alias FrontendModel =
    { clientId : Lamdera.ClientId
    , errors : (List ErrorType)
    , gameId : GameId
    , playerName : PlayerId
    , isSpectator : Bool
    , truths : (List Answer)
    , lie : Answer
    , currentGuess : (Maybe Int)
    , guessText : (Maybe String)
    , game : (Maybe Game)
    , allPlayers : PlayerMapping
    , correctAnswerForRound : String
    }


type alias ClientMap = (Dict.Dict GameId (List Lamdera.ClientId))


type alias BackendModel =
    { clients : (Set.Set Lamdera.ClientId)
    , clientMap : ClientMap
    , games : (Dict.Dict GameId Game)
    , players : PlayerMapping
    , playerClientMap : (Dict.Dict PlayerId Lamdera.ClientId)
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
    | SetNextPlayer Game ((Maybe PlayerId), (List PlayerId))
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