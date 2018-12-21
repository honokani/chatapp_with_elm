module EchoBack.Session exposing (..)
-- common modules
import Browser.Navigation as Nav
-- my modules
import EchoBack.Page.Chat.ChatInfo exposing (..)

type alias Usr = String
type UsrCtrl = SS  Nav.Key Usr
             | GSS Nav.Key

type alias HomeSt    = String
type alias CounterSt = Int
type alias ChatSt    = List ChatInfo

type alias Session = { uCtrl     : UsrCtrl
                     , stHome    : Maybe HomeSt
                     , stCounter : Maybe CounterSt
                     , stChat    : Maybe ChatSt
                     }

getInitialSession : Nav.Key -> Session
getInitialSession k = Session (GSS k) Nothing Nothing Nothing

navKey : Session -> Nav.Key
navKey ss = case ss.uCtrl of
    SS  k _ -> k
    GSS k   -> k

