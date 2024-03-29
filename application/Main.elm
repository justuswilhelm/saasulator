module Main exposing (..)

import Model
import Html
import Update
import View
import Msg
import Cmds
import Subscriptions


main : Program Never Model.Model Msg.Msg
main =
    Html.program
        { init = ( Model.init, Cmds.startCmds )
        , update = Update.update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        }
