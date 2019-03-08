port module EchoBack.Auth exposing (..)

import EchoBack.Port as Po exposing (..)
import EchoBack.Session    exposing (Session, updateUsrRank)

import EchoBack.Model      exposing (..)

type Msg = SignIn
         | SignOut
         | SignedIn (Maybe String)

update : Msg -> Model -> (Model, Cmd msg)
update msg mdl = case msg of
    SignIn ->
        ( mdl, Po.signIn () )
    SignOut ->
        ( mdl, Po.signOut () )
    SignedIn mayStr ->
        let
            ss = getSession mdl
            newSs = {ss | uCtrl = updateUsrRank mayStr ss.uCtrl }
            newMdl = setSession mdl newSs
        in
            ( newMdl, Cmd.none )


getSession : Model -> Session
getSession mdl = case mdl of
    MdlHome mhm -> mhm.session
    MdlCntr mcn -> mcn.session
    MdlChat mch -> mch.session
--    MdlAuth mau -> getSession mau

setSession mdl ss = case mdl of
    MdlHome mhm -> MdlHome { mhm | session = ss }
    MdlCntr mcn -> MdlCntr { mcn | session = ss }
    MdlChat mch -> MdlChat { mch | session = ss }
--    _           -> mdl
