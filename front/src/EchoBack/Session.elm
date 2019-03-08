module EchoBack.Session exposing (..)
-- common modules
import Browser.Navigation as Nav
-- my modules
import EchoBack.Page.Chat.ChatInfo exposing (..)


type alias Usr = String
type UsrCtrl = SS Nav.Key Usr
             | GS Nav.Key

type alias HomeSt    = String
type alias CounterSt = Int
type alias ChatSt    = List ChatInfo

type alias Session = { uCtrl     : UsrCtrl
                     , stHome    : Maybe HomeSt
                     , stCounter : Maybe CounterSt
                     , stChat    : Maybe ChatSt
                     }


getInitialSession : Nav.Key -> Session
getInitialSession k = Session (GS k) Nothing Nothing Nothing

navKey : Session -> Nav.Key
navKey ss = case ss.uCtrl of
    SS k _ -> k
    GS k   -> k

updateUsrRank mnu uc = case mnu of
    Nothing -> case uc of
        SS k u -> GS k
        _      -> uc
    Just nu -> case uc of
        GS k -> SS k nu
        _    -> uc

getUsr ss = case ss.uCtrl of
    SS _ u -> Just u
    _      -> Nothing


