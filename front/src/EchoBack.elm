module EchoBack exposing (Model, Msg, program)
-- common modules
import Browser               as Brs
import Browser.Navigation    as Nav
import Html                           exposing (..)
import Html.Attributes                exposing (..)
import Url                            exposing (Url)
-- my modules
import EchoBack.Flags                 exposing (Flags)
import EchoBack.Route        as R     exposing (..)
import EchoBack.Session               exposing (Session, UsrCtrl(..), navKey, getInitialSession)
import EchoBack.Page.Home    as PHome exposing (Msg, Model, init, update, view)
import EchoBack.Page.Counter as PCntr exposing (Msg, Model, init, update, view)
import EchoBack.Page.Chat    as PChat exposing (Msg, Model, init, update, view)


type Msg = LinkClicked Brs.UrlRequest
         | UrlChanged  Url.Url
         | MsgHome     PHome.Msg
         | MsgCntr     PCntr.Msg
         | MsgChat     PChat.Msg

type Model = MdlHome PHome.Model
           | MdlCntr PCntr.Model
           | MdlChat PChat.Model

type alias BaseModel = { session : Session }



program : Program Flags Model Msg
program = Brs.application { init = init
                          , update = update
                          , subscriptions = subscriptions
                          , view = view
                          , onUrlChange = UrlChanged
                          , onUrlRequest = LinkClicked
                          }



init : Flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ u k =
    let
        (subMdl, subMsg) = PHome.init (getInitialSession k)
    in
        (MdlHome subMdl, Cmd.map MsgHome subMsg)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl = case msg of
    LinkClicked urlReq -> case urlReq of
        Brs.Internal url ->
            let
                changeUrlCmd = Nav.pushUrl (navKey (getSession mdl)) (Url.toString url)
            in
                updateInternalPage url mdl |> changeUrl [changeUrlCmd]
        Brs.External href -> ( mdl , Nav.load href )
    UrlChanged url ->
        updateInternalPage url mdl
    MsgHome  subMsg -> case mdl of
        MdlHome subMdl -> updateWithSub MdlHome MsgHome (PHome.update subMsg subMdl)
        _              -> ( mdl, Cmd.none )
    MsgCntr subMsg -> case mdl of
        MdlCntr subMdl -> updateWithSub MdlCntr MsgCntr (PCntr.update subMsg subMdl)
        _              -> ( mdl, Cmd.none )
    MsgChat  subMsg -> case mdl of
        MdlChat subMdl -> updateWithSub MdlChat MsgChat (PChat.update subMsg subMdl)
        _              -> ( mdl, Cmd.none )


getSession : Model -> Session
getSession mdl = case mdl of
    MdlHome mhm -> mhm.session
    MdlCntr mcn -> mcn.session
    MdlChat mch -> mch.session


updateInternalPage : Url.Url -> Model -> ( Model, Cmd Msg )
updateInternalPage url mdl =
    let
        ss = getSession mdl
    in case R.fromUrl url of
        Just Root    ->
            updateWithSub MdlHome MsgHome (PHome.init ss)
        Just Home    ->
            updateWithSub MdlHome MsgHome (PHome.init ss)
        Just Counter ->
            updateWithSub MdlCntr MsgCntr (PCntr.init ss)
        Just Chat    ->
            updateWithSub MdlChat MsgChat (PChat.init ss)
        _            ->
            ( mdl, Cmd.none )


updateWithSub : (subMdl -> Model) -> (subMsg -> Msg) -> (subMdl, Cmd subMsg) -> (Model, Cmd Msg)
updateWithSub toMdl toMsg (subMdl, subCmd) = (toMdl subMdl, Cmd.map toMsg subCmd)


changeUrl : List (Cmd Msg) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
changeUrl cmds (mdl, cmd) = (mdl, Cmd.batch <| cmd :: cmds)



subscriptions : Model -> Sub Msg
subscriptions mdl = case mdl of
    MdlHome subMdl -> Sub.none
    MdlCntr subMdl -> Sub.map MsgCntr (PCntr.subscriptions subMdl)
    MdlChat subMdl -> Sub.map MsgChat (PChat.subscriptions subMdl)



view : Model -> Brs.Document Msg
view mdl =
    let
        skelton  = case mdl of
            MdlHome subMdl -> Html.map MsgHome <| PHome.view subMdl
            MdlCntr subMdl -> Html.map MsgCntr <| PCntr.view subMdl
            MdlChat subMdl -> Html.map MsgChat <| PChat.view subMdl
    in
        { title = "Echo back"
        , body  = [make_body skelton]
        }


make_body : Html Msg -> Html Msg
make_body con =
    let
        head_html = [ h1 [] [ link "/" "EchoBack" ] ]
        foot_html = [ text "test for chatbot app!" ]
        navi_html = [ p [] [ link ""         "Home" ]
                    , p [] [ link "#counter" "Counter" ]
                    , p [] [ link "#chat"    "Chat" ]
                    ]
    in
        div [ class "root" ]
            [ bootFluid header <| Anone head_html
            , bootFluid div    <| Named ["page"]
                                    <| bootGrid [ ( 1, Named ["navi"] navi_html)
                                                , (11, Named ["content"] [con])
                                                ]
            , bootFluid footer <| Anone foot_html
            ]

link : String -> String -> Html msg
link url label = a [ href url ] [ text label ]



type alias Htmls a = List (Html a)
type BootStrapHtmlSet msg = Anone               (Htmls msg)
                          | Named (List String) (Htmls msg)

bootFluid : (List (Attribute msg) -> Htmls msg -> Html msg) -> BootStrapHtmlSet msg -> Html msg
bootFluid tag e = case e of
    Anone    hs -> tag [class "container-fluid"] hs
    Named ns hs -> tag [class <| packName ns ++ "container-fluid"] hs

packName : List String -> String
packName ns = List.foldr (\x acc -> acc ++ x ++ " ") "" ns

type alias BootStrapGrid msg = (Int, BootStrapHtmlSet msg)

bootGrid : List (BootStrapGrid msg) -> List (Html msg)
bootGrid elems =
    let
        bootCols : List (BootStrapGrid msg) -> List (Html msg)
        bootCols = List.map bootCol
        bootCol : (BootStrapGrid msg) -> Html msg
        bootCol (i, e) = case e of
            Anone    hs -> div [class <| "col-" ++ String.fromInt i] hs
            Named ns hs -> div [class <| packName ns ++ "col-" ++ String.fromInt i] hs
    in
        [div [class "row h-100"] <| bootCols elems]

bootNavV elem = nav [class "flex-column nav-pills"] elem

