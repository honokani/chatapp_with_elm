module EchoBack.Page.Chat exposing (..)
-- common modules
import Html                        exposing (..)
import Html.Attributes             exposing (..)
import Html.Events                 exposing (onClick, onSubmit, onInput)
-- my modules
import EchoBack.Session            exposing (Session, CounterSt)
import EchoBack.Page.Chat.ChatInfo exposing (..)


type alias Model = { session : Session
                   , aaa : String
                   , chatLog : List ChatInfo
                   }

type Msg = PostTxt
         | ChangeTxt String


init : Session -> ( Model, Cmd Msg )
init ss = (Model ss "" <| Maybe.withDefault initializeChatlog ss.stChat, Cmd.none)

initializeChatlog = [ISay "isay", USay "usay"]


update msg mdl = case msg of
    ChangeTxt s ->
        ({mdl| aaa=s}, Cmd.none)
    PostTxt ->
        let
            input = mdl.aaa
            inputRev = String.reverse mdl.aaa
        in
        ({mdl| aaa="", chatLog=(USay inputRev::ISay input::mdl.chatLog)}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


view : Model -> Html Msg
view mdl =
    let
        subTitleHtml = h1 [] [ text "Chat" ]
        chatLine = List.map convertChat2Div mdl.chatLog
    in
        div [ class ""]
            [ subTitleHtml
            , Html.form [ class "inputtext", onSubmit PostTxt ]
                        [ input [ placeholder "Enter some Question here!"
                                , onInput  ChangeTxt
                                , value mdl.aaa
                                , rows 10
                                , class "inputtext"
                                ] []
                        ]
            , div [ class "chatlog" ] chatLine
            ]

convertChat2Div : ChatInfo -> Html Msg
convertChat2Div c = case c of
    ISay s -> makeBaloonDiv "I" <| div [class "isay"] [makeCommentAreaDiv s]
    USay s -> makeBaloonDiv "U" <| div [class "usay"] [makeCommentAreaDiv s]

makeBaloonDiv : String -> Html Msg -> Html Msg
makeBaloonDiv s h = div [class <| "baloon" ++ s] [h]

makeCommentAreaDiv : String -> Html Msg
makeCommentAreaDiv s = div [class "comment"] [text s]

