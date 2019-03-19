module EchoBack.Session exposing (..)
-- common modules
import Browser.Navigation        as Nav
import EchoBack.Page.Chat.Baloon        exposing (..)


type alias Usr = String
type UsrCtrl = SS Nav.Key Usr
             | GS Nav.Key

type alias HomeSt    = String
type alias CounterSt = Int
type alias ChatSt    = List BaloonInfo

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

changeUsrRank mayNewUsr uc = case mayNewUsr of
    Nothing -> case uc of
        SS k u -> GS k
        _      -> uc
    Just newUsr -> case uc of
        GS k -> SS k newUsr
        _    -> uc

getUsr ss = case ss.uCtrl of
    SS _ u ->
        Just u
    _      ->
        Nothing

login usr ss = case ss.uCtrl of
    GS k ->
        { ss | uCtrl = SS k usr }
    _ ->
        ss

logout ss = case ss.uCtrl of
    GS k ->
        { ss | uCtrl = GS k }
    SS k _ ->
        { ss | uCtrl = GS k }

