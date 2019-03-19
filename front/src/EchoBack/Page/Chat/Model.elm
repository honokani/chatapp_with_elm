module EchoBack.Page.Chat.Model exposing (..)

import EchoBack.Session          exposing (Session, CounterSt, Usr)
import EchoBack.Page.Chat.Baloon exposing (..)


type alias Model = { session    : Session
                   , inputText  : String
                   , chatLog    : List BaloonInfo
                   , latestTime : Maybe Int
                   }

