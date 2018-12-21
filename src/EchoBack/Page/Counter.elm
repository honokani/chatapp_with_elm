module EchoBack.Page.Counter exposing (..)
-- common modules
import Html                    exposing (..)
import Html.Attributes         exposing (..)
import Html.Events             exposing (onClick)
-- my modules
import EchoBack.Session        exposing (Session, CounterSt)


type alias Model = { session : Session
                   , count   : CounterSt
                   }

type Msg = Increment
         | Decrement

--counterMain : Program Session Model Msg
--counterMain = Brs.element { init   = init
--                          , update = update
--                          , subscriptions = subscriptions
--                          , view   = view
--                          }


init : Session -> ( Model, Cmd Msg )
init ss = ( Model ss <| Maybe.withDefault 0 ss.stCounter, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    let
        newCnt = case msg of
            Increment -> mdl.count + 1
            Decrement -> mdl.count - 1
        ss = mdl.session
        newMdl = Model { ss | stCounter = Just newCnt } newCnt
    in
        (newMdl , Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


view : Model -> Html Msg
view mdl =
    let
        subTitleHtml = h1  [] [ text "Counter" ]
        val = String.fromInt mdl.count
    in
        div [ class "counter" ]
            [ subTitleHtml
            , p  [] [ button [ onClick Decrement ] [ text "-" ] ]
            , p  [] [ div    []                    [ text val ] ]
            , p  [] [ button [ onClick Increment ] [ text "+" ] ]
            ]

