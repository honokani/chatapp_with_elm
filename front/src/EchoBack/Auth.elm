port module EchoBack.Auth exposing (..)

import EchoBack.Port as Po exposing (..)
import EchoBack.Session    exposing (Session, changeUsrRank)

import EchoBack.Model      exposing (..)

type Msg = SignIn
         | SignOut
         | CheckRank (Maybe String)

update : Msg -> Model -> (Model, Cmd msg)
update msg mdl = case msg of
    SignIn ->
        ( mdl, Po.signIn () )
    SignOut ->
        ( mdl, Po.signOut () )
    CheckRank mayStr ->
        let
            ss = getSession mdl
            newSs = {ss | uCtrl = changeUsrRank mayStr ss.uCtrl }
            newMdl = setSession mdl newSs
        in
            ( newMdl, Cmd.none )


getSession : Model -> Session
getSession mdl = case mdl of
    MdlHome mhm -> mhm.session
    MdlCntr mcn -> mcn.session
    MdlChat mch -> mch.session

setSession mdl ss = case mdl of
    MdlHome mhm -> MdlHome { mhm | session = ss }
    MdlCntr mcn -> MdlCntr { mcn | session = ss }
    MdlChat mch -> MdlChat { mch | session = ss }

