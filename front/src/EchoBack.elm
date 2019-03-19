module EchoBack exposing (Model, Msg, program)
-- common modules
import Browser               as Brs
import Browser.Navigation    as Nav
import Html                            exposing (..)
import Html.Attributes                 exposing (..)
import Html.Events                     exposing (onClick)
import Http
import Url                             exposing (Url)
import Json.Encode           as JE
import Time
import Task
-- my modules
import EchoBack.Model        as M      exposing (..)
import EchoBack.Flags                  exposing (Flags)
import EchoBack.Auth         as Au     exposing (..)
import EchoBack.Port         as Po     exposing (..)
import EchoBack.Route        as Rt     exposing (..)
import EchoBack.Session                exposing (Session, UsrCtrl(..))
import EchoBack.Session                exposing (navKey, getInitialSession, getUsr, login, logout)
import EchoBack.Page.Home    as PgHome exposing (Msg, init, update, view)
import EchoBack.Page.Counter as PgCntr exposing (Msg, init, update, view)
import EchoBack.Page.Chat    as PgChat exposing (Msg, init, update, view)

type alias Model = M.Model

type Msg
    = LinkClicked Brs.UrlRequest
    | UrlChanged  Url.Url
    | MsgHome     PgHome.Msg
    | MsgCntr     PgCntr.Msg
    | MsgChat     PgChat.Msg
    | MsgAuth     Au.Msg
    | DevLogIn
    | DevLogOut



program : Program Flags Model Msg
program = Brs.application
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlChange   = UrlChanged
    , onUrlRequest  = LinkClicked
    }


init : Flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ u k =
    let
        (subMdl, subMsg) = PgHome.init (getInitialSession k)
    in
        (MdlHome subMdl, Cmd.map MsgHome subMsg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl = case msg of
    LinkClicked urlReq ->
        case urlReq of
            Brs.Internal url ->
                let
                    changeUrlCmd = Nav.pushUrl (navKey (getSession mdl)) (Url.toString url)
                in
                    updateInternalPage url mdl |> changeUrl [changeUrlCmd]
            Brs.External href ->
                ( mdl , Nav.load href )

    UrlChanged url ->
        updateInternalPage url mdl

    MsgHome subMsg -> case mdl of
        MdlHome subMdl ->
            updateWithSub MsgHome MdlHome <| PgHome.update subMsg subMdl
        _ ->
            ( mdl, Cmd.none )

    MsgCntr subMsg -> case mdl of
        MdlCntr subMdl ->
            updateWithSub MsgCntr MdlCntr <| PgCntr.update subMsg subMdl
        _ ->
            ( mdl, Cmd.none )

    MsgChat subMsg -> case mdl of
        MdlChat subMdl ->
            updateWithSub MsgChat MdlChat <| PgChat.update subMsg subMdl
        _ ->
            ( mdl, Cmd.none )

    MsgAuth subMsg ->
        Au.update subMsg mdl

    DevLogIn ->
        ( setSession (login "testUsr" <| getSession mdl) mdl, Cmd.none )
    DevLogOut ->
        ( setSession (logout <| getSession mdl) mdl, Cmd.none )


getSession : Model -> Session
getSession mdl = case mdl of
    MdlHome subModel -> subModel.session
    MdlCntr subModel -> subModel.session
    MdlChat subModel -> subModel.session

setSession ss mdl = case mdl of
    MdlHome subModel -> MdlHome {subModel | session=ss}
    MdlCntr subModel -> MdlCntr {subModel | session=ss}
    MdlChat subModel -> MdlChat {subModel | session=ss}



updateInternalPage : Url.Url -> Model -> ( Model, Cmd Msg )
updateInternalPage url mdl =
    let
        ss = getSession mdl
    in case Rt.fromUrl url of
        Just Root    ->
            updateWithSub MsgHome MdlHome (PgHome.init ss)
        Just Home    ->
            updateWithSub MsgHome MdlHome (PgHome.init ss)
        Just Counter ->
            updateWithSub MsgCntr MdlCntr (PgCntr.init ss)
        Just Chat    ->
            updateWithSub MsgChat MdlChat (PgChat.init ss)
        _            ->
            ( mdl, Cmd.none )


updateWithSub : (subMsg -> Msg) -> (subMdl -> Model) -> (subMdl, Cmd subMsg) -> (Model, Cmd Msg)
updateWithSub toMsg toMdl (subMdl, subCmd) = (toMdl subMdl, Cmd.map toMsg subCmd)


changeUrl : List (Cmd Msg) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
changeUrl cmds (mdl, cmd) = (mdl, Cmd.batch <| cmd :: cmds)



subscriptions : Model -> Sub Msg
subscriptions mdl = case mdl of
    MdlChat subMdl ->
        Sub.map MsgChat (PgChat.subscriptions subMdl)
    _ ->
        Sub.batch [ Po.checkSign <| MsgAuth << Au.CheckRank
                  ]



view : Model -> Brs.Document Msg
view mdl =
    let
        ss = getSession mdl
        content = case mdl of
            MdlHome subMdl -> Html.map MsgHome <| PgHome.view subMdl
            MdlCntr subMdl -> Html.map MsgCntr <| PgCntr.view subMdl
            MdlChat subMdl -> Html.map MsgChat <| PgChat.view subMdl
    in
        { title = "Echo back"
        , body  = [make_body ss content]
        }


make_body : Session -> Html Msg -> Html Msg
make_body ss con =
    let
        usr = getUsr ss
        (signIOsub, appNav, appFoot) = case usr of
            Nothing ->
                let
                    logIO = button [class "id_mem", onClick DevLogIn][text "Log in"]
                in
                    ( div [ class "sign_io", onClick SignIn  ] [text "Google SignIn"]
                    , nav [ ] [ p [class "id_mem"] [ link ""         "Home" ]
                              , p [class "id_mem"] [ link "#counter" "Counter" ]
                              , logIO
                              ]
                    , footer [] [ text <| "hi guest user! this is test for chatbot app!" ]
                    )
            Just uId ->
                let
                    logIO = button [class "id_mem", onClick DevLogOut] [text "Log out"] 
                in
                    ( div [ class "sign_io", onClick SignOut ] [text "Google SignOut"]
                    , nav [ ] [ p [class "id_mem"] [ link ""         "Home" ]
                              , p [class "id_mem"] [ link "#counter" "Counter" ]
                              , p [class "id_mem"] [ link "#chat"    "Chat" ]
                              , logIO
                              ]
                    , footer [] [ text <| "hi " ++ uId ++ "!" ]
                    )

        humbergerMock = div [ class "humberger"] [text "三"]
        signIO   = Html.map MsgAuth signIOsub
        appHead  = header [] [ h1  [class "page_title"] [ link "/" "EchoBack" ]
                             , signIO
                             , humbergerMock
                             ]
        page = div [class "page"] [con]
        container = div [ class "container" ] [ appNav
                                              , page
                                              ]
    in
        div [ class "root" ] [ appHead
                             , container
                             , appFoot
                             ]


link : String -> String -> Html msg
link url label = a [ href url ] [ text label ]

