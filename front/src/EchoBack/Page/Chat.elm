module EchoBack.Page.Chat exposing (..)
-- common modules
import Html                        exposing (..)
import Html.Attributes             exposing (..)
import Html.Events                 exposing (onClick, onSubmit, onInput)
import Http                        exposing (..)
-- my modules
import EchoBack.Session            exposing (Session)
import EchoBack.Page.Chat.Model    exposing (..)
import EchoBack.Page.Chat.ChatInfo exposing (..)


type Msg = ChangeText String
         | PostText
         | GotText (Result Http.Error String)



init : Session -> ( Model, Cmd Msg )
init ss = (Maybe.withDefault initializeChatlog ss.stChat |> Model ss "" , Cmd.none)

initializeChatlog = [USay "Ask me anything!!"]



update msg mdl = case msg of
    ChangeText s -> ({mdl| inputText=s}, Cmd.none)
    PostText ->
        let
            ss = mdl.session
            it = mdl.inputText
            newLog = ISay it::mdl.chatLog
            reqCmd = begReversedTxt it
        in
            ( {mdl | session   = {ss| stChat=Just newLog }
                   , inputText = ""
                   , chatLog   = newLog
              }
            , reqCmd
            )
    GotText (Ok s) ->
        let
            ss = mdl.session
            newLog = (USay s::mdl.chatLog)
        in
            ( {mdl | session = {ss| stChat=Just newLog }
                   , chatLog = newLog
              }
            , Cmd.none
            )
    GotText (Err _) ->
        ( {mdl| chatLog=(USay "... ..."::mdl.chatLog)}, Cmd.none)


begReversedTxt : String -> Cmd Msg
begReversedTxt s =
    let
        apiurl = String.join "/" ["http://localhost:3030/rvs_str", s]
    in
        Http.get { url = apiurl, expect = Http.expectString GotText }



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
            , Html.form [ class "inputtext", onSubmit PostText ]
                        [ input [ placeholder "Enter some Question here!"
                                , onInput  ChangeText
                                , value mdl.inputText
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

