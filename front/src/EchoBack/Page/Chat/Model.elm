module EchoBack.Page.Chat.Model exposing (..)

import EchoBack.Page.Chat.ChatInfo exposing (..)
import EchoBack.Session            exposing (Session, CounterSt)

type alias Model = { session   : Session
                   , inputText : String
                   , chatLog   : List ChatInfo
                   }

