module EchoBack.Page.Chat.Msg exposing (..)

import Http exposing (..)

type Msg
    = ChangeText String
    | InitializeLog (Result Http.Error String)
    | PostingText
    | GotResOfPost (Result Http.Error String)
    | CheckLaterText
    | GotLaterText (Result Http.Error String)
    | DevPostYA
    | DevGot (Result Http.Error String)

