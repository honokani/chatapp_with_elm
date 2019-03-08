module EchoBack.Page.Home exposing (..)
-- common modules
import Html                     exposing (..)
import Html.Attributes          exposing (..)
-- my modules
import EchoBack.Session         exposing (Session)
import EchoBack.Page.Home.Model exposing (..)


type Msg = ChangeTxt String


init : Session -> ( Model, Cmd Msg )
init ss =
    let
        initTxt = case ss.stHome of
            Nothing -> "hello world!!!!!"
            Just s  -> s
    in
        Debug.log("home")
        ( { session=ss, txt=initTxt }, Cmd.none )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl = case msg of
    ChangeTxt a -> (Model mdl.session a, Cmd.none)



view : Model -> Html Msg
view mdl =
    let
        val = mdl.txt
    in
        div [ class "home" ]
            [ h1 [] [ text "Home" ]
            , p  [] [ div  [] [text val] ]
            ]

