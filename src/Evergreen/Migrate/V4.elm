module Evergreen.Migrate.V4 exposing (..)

import Dict
import Evergreen.V1.Types as Old
import Lamdera.Migrations exposing (..)
import Set
import Evergreen.V4.Types as New


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { clientId = ""
          , errors = [ New.Fatal "MIGRATION HAPPENED, START OVER" ]
          , gameId = ""
          , playerName = ""
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
    ModelMigrated
        ( { clients = Set.empty
          , clientMap = Dict.empty
          , games = Dict.empty
          , players = Dict.empty
          , playerClientMap = Dict.empty
          }
        , Cmd.none
        )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
