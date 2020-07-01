module Evergreen.Migrate.V5 exposing (..)

import Dict
import Evergreen.V4.Types as Old
import Lamdera.Migrations exposing (..)
import Evergreen.V5.Types as New


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { clientId = ""
          , errors = [ New.Fatal "MIGRATION HAPPENED, START OVER" ]
          , gameId = ""
          , playerName = ""
          , isSpectator = False
          , truths = []
          , lie = New.Answer "" New.Lie
          , currentGuess = Nothing
          , guessText = Nothing
          , game = Nothing
          , allPlayers = Dict.empty
          , correctAnswerForRound = ""
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    case old of
        Old.ClientJoin ->
            MsgUnchanged

        Old.CreateGame gameId playerId ->
            MsgMigrated ( New.CreateGame gameId playerId False, Cmd.none )

        Old.AddPlayerToGame gameId playerId ->
            MsgMigrated ( New.AddPlayerToGame gameId playerId False, Cmd.none )

        Old.LockPlayerAnswers _ _ _ _ ->
            MsgUnchanged

        Old.StartTheGame _ ->
            MsgUnchanged

        Old.SubmitPlayerGuess _ _ _ ->
            MsgUnchanged

        Old.NextRoundForGame _ ->
            MsgUnchanged

        Old.DeletePlayer _ _ ->
            MsgUnchanged

        Old.DeleteGame _ ->
            MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    case old of
        Old.Error _ ->
            MsgUnchanged

        Old.GameCreated _ _ ->
            MsgUnchanged

        Old.PlayerJoinedTF game players ->
            let
                newGame =
                    { activePlayer = Nothing
                    , currentAnswers = []
                    , currentGuesses = Dict.empty
                    , hostPlayer = ""
                    , id_ = ""
                    , playerTurnOver = []
                    , players = []
                    , state = New.Initial
                    }
            in
            MsgMigrated ( New.PlayerJoinedTF newGame Dict.empty False, Cmd.none )

        Old.PlayerUpdated _ ->
            MsgUnchanged

        Old.LastPlayerLocked _ _ ->
            MsgUnchanged

        Old.NextRoundReady _ _ ->
            MsgUnchanged

        Old.PlayerGuessed _ _ ->
            MsgUnchanged

        Old.AnswerWithUpdatedScores _ _ _ ->
            MsgUnchanged

        Old.GameOver _ _ ->
            MsgUnchanged

        Old.PlayerDeleted _ _ _ ->
            MsgUnchanged

        Old.GameDeleted ->
            MsgUnchanged
