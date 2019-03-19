module EchoBack.Page.Chat exposing (..)
-- common modules
import Html                              exposing (..)
import Html.Attributes                   exposing (..)
import Html.Events                       exposing (onClick, onSubmit, onInput)
import Http                              exposing (..)
import Time
-- my modules
import EchoBack.Session            as SS exposing (..)
import EchoBack.Page.Chat.Msg      as CM exposing (..)
import EchoBack.Page.Chat.Model          exposing (..)
import EchoBack.Page.Chat.Baloon         exposing (..)
import EchoBack.Page.Chat.PostText       exposing (..)


type alias Msg = CM.Msg


init : Session -> ( Model, Cmd Msg )
init ss =
    let
        requestLogLen = 10
        initAct = case ss.stChat of
            Nothing ->
                rcvLatestChat { rCnt=requestLogLen }
            _       ->
                Cmd.none
    in
        ( Model ss "" [] Nothing, initAct )



update : CM.Msg -> Model -> (Model, Cmd CM.Msg)
update msg mdl = case msg of
    ChangeText s ->
        ( {mdl| inputText=s}, Cmd.none )

    InitializeLog res -> case res of
        Ok s -> case getUsr mdl.session of
            Nothing ->
                ( mdl, Cmd.none )
            Just u ->
                let
                    (initLog, lt) = analyseLog s u mdl.latestTime
                in
                    ( { mdl | chatLog=initLog , latestTime=lt }
                    , Cmd.none
                    )
        Err e -> case e of
            Http.BadUrl s ->
                ( mdl, Cmd.none )
            _ ->
                ( mdl, Cmd.none )

    PostingText -> case getUsr mdl.session of
        Nothing ->
            ( mdl, Cmd.none )
        Just u ->
            let
                postData = PostedText u <| mdl.inputText
                newMdl = { mdl | inputText = "" }
            in
                ( newMdl, sndText postData)

    GotResOfPost res -> case res of
        Ok _ ->
            update CheckLaterText mdl
        Err _ ->
            ( mdl, Cmd.none )

    CheckLaterText -> case mdl.latestTime of
        Nothing ->
            ( mdl, Cmd.none )
        Just n ->
            ( mdl, getTextLater <| LatestTime n )

    GotLaterText res -> case res of
        Ok s -> case getUsr mdl.session of
            Nothing ->
                ( mdl, Cmd.none )
            Just u ->
                let
                    (laterLog, lt) = analyseLog s u mdl.latestTime
                    newLog = List.append laterLog mdl.chatLog
                in
                    ( { mdl | chatLog=newLog , latestTime=lt }
                    , Cmd.none
                    )
        Err e -> case e of
            Http.BadUrl s ->
                ( mdl, Cmd.none )
            _ ->
                ( mdl, Cmd.none )

    DevPostYA -> case getUsr mdl.session of
        Nothing ->
            ( mdl, Cmd.none )
        Just u ->
            let
                postData = PostedText "Another" <| mdl.inputText
                newMdl = { mdl | inputText = "" }
            in
                ( newMdl, sndTextDev postData)
    DevGot res ->
        ( mdl, Cmd.none )

analyseLog gotStr usr oldT =
    let
        logRaw = case decodeChatLog gotStr of
            Ok x  -> x
            Err _ -> []
        logRawT = Maybe.map getTime <| List.head logRaw
        newT = case logRawT of
            Nothing ->
                oldT
            _ ->
                logRawT
    in
        ( List.map (structInfo usr ) logRaw , newT )



subscriptions : Model -> Sub Msg
subscriptions mdl =
    checkLatestPostPer1sec

checkLatestPostPer1sec = checkLatestPost 1000

checkLatestPost ms = 
    Time.every ms (always CheckLaterText)

view : Model -> Html Msg
view mdl = case SS.getUsr mdl.session of
    Nothing ->
        let
            subTitleHtml = h1 [class "subtitle"] [ text "Chat Room! but guest" ]
        in
            div [class "chat"] [subTitleHtml]
    Just usr ->
        viewForUsr mdl



viewForUsr : Model -> Html Msg
viewForUsr mdl =
    let
        initialMessage = "Enter any Comments here!"
        subTitleHtml = h1 [class "subtitle"] [ text "Chat" ]
        gap = div [class "gap"] []
        chatLineRow = List.take 30 <| List.map convertChat2Div mdl.chatLog
        chatLine = interposeGap chatLineRow gap
        chatTest = div [onClick DevPostYA] [text "YetAnotherMemberPost"]
    in
        div [class "chat"]
            [ subTitleHtml
            , div [ class "chatlog_o" ] [div [class "chatlog_i"] chatLine]
            , Html.form [ class "inputarea", onSubmit PostingText ]
                        [ input [ placeholder initialMessage
                                , onInput  ChangeText
                                , value mdl.inputText
                                , class "inputtext"
                                ] []
                        ]
            , chatTest
            ]

interposeGap ls gap = case ls of
    [] ->
        [gap]
    (x::xs) ->
        gap::x::interposeGap xs gap



convertChat2Div : BaloonInfo -> Html Msg
convertChat2Div c = 
    let
        icon = div [class "icon"] []
        appendIcon e = [e, icon]
    in case c of
        ISay s t ->
            makeBaloonDiv "I" <| appendIcon <| div [class "baloon_i"] [makeCommentAreaDiv s t]
        USay s t ->
            makeBaloonDiv "U" <| appendIcon <| div [class "baloon_u"] [makeCommentAreaDiv s t]

makeBaloonDiv : String -> List (Html Msg) -> Html Msg
makeBaloonDiv s h = div [class <| s ++ "say"] h

makeCommentAreaDiv : String -> String -> Html Msg
makeCommentAreaDiv s t = div [class "res"] [ div [class "comment"] [text s]
                                           , div [class "c_time"] [text t]
                                           ]

