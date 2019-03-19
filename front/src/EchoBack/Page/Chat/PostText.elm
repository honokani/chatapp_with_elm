module EchoBack.Page.Chat.PostText exposing (..)

import Json.Encode            as JE
import Http                              exposing (..)
import EchoBack.Page.Chat.Msg          exposing (..)



type alias RcvChatlog = { rCnt : Int }

encodeForRcvCnt : RcvChatlog -> JE.Value
encodeForRcvCnt n = JE.object [ ("count" , JE.int n.rCnt) ]

rcvLatestChat rcv =
    let
        url = "http://dummy.cloudfunctions.net/getMessage"
    in
        Http.post { url    = url
                  , body   = Http.jsonBody <| encodeForRcvCnt rcv
                  , expect = Http.expectString InitializeLog
                  }



type alias PostedText =
    { cSender  : String
    , cMessage : String
    }

encodeForChatPost : PostedText -> JE.Value
encodeForChatPost cp = JE.object [ ("sender" , JE.string cp.cSender)
                                 , ("message", JE.string cp.cMessage)
                                 ]

sndText pData =
    let
        url = "http://dummy.cloudfunctions.net/addMessage"
    in
        Http.post { url    = url
                  , body   = Http.jsonBody <| encodeForChatPost pData
                  , expect = Http.expectString GotResOfPost
                  }



type alias LatestTime =
    { lNum : Int }

encodeForGetLater : LatestTime -> JE.Value
encodeForGetLater gl = JE.object [ ("number" , JE.int gl.lNum) ]

getTextLater lData =
    let
        url = "http://dummy.cloudfunctions.net/getMessageLater"
    in
        Http.post { url    = url
                  , body   = Http.jsonBody <| encodeForGetLater lData
                  , expect = Http.expectString GotLaterText
                  }

sndTextDev pData =
    let
        url = "http://dummy.cloudfunctions.net/addMessage"
    in
        Http.post { url    = url
                  , body   = Http.jsonBody <| encodeForChatPost pData
                  , expect = Http.expectString DevGot
                  }

