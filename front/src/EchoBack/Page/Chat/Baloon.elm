module EchoBack.Page.Chat.Baloon exposing (..)

import Time
import Json.Decode           as JD


type alias Baloons = { res : List Baloon }

type alias Baloon =
    { bSender : String
    , bTime : Int
    , bMessage : String
    }
getTime b = b.bTime
type BaloonInfo
    = ISay String String
    | USay String String


decodeBaloons : JD.Decoder (List Baloon)
decodeBaloons
    = JD.field "messages" (JD.list decodeBaloon)

decodeBaloon : JD.Decoder Baloon
decodeBaloon
    = JD.map3 Baloon
        (JD.field "sender" JD.string)
        (JD.field "number" JD.int)
        (JD.field "message" JD.string)

decodeChatLog = JD.decodeString (JD.list decodeBaloon)

structInfo : String -> Baloon -> BaloonInfo
structInfo u b =
    let
        identify = if u == b.bSender then ISay else USay
    in
        identify b.bMessage <| millisecToDttmstr b.bTime

millisecToDttmstr : Int -> String
millisecToDttmstr m = makeDttmstr <| Time.millisToPosix m

makeDttmstr : Time.Posix -> String
makeDttmstr posix =
    let
        z = Time.utc
        dStr = String.fromInt <| Time.toDay z posix
        mStr = monthToStr <| Time.toMonth z posix
        yStr = String.fromInt <| Time.toYear z posix
    in
        String.join " " [dStr,  mStr, yStr]

monthToStr : Time.Month -> String
monthToStr m = case m of
    Time.Jan -> "Jan"
    Time.Feb -> "Feb"
    Time.Mar -> "Mar"
    Time.Apr -> "Apr"
    Time.May -> "May"
    Time.Jun -> "Jun"
    Time.Jul -> "Jul"
    Time.Aug -> "Aug"
    Time.Sep -> "Sep"
    Time.Oct -> "Oct"
    Time.Nov -> "Nov"
    Time.Dec -> "Dec"

