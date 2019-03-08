module EchoBack.Page.Home.Model exposing (..)

import EchoBack.Session exposing (Session, HomeSt)

type alias Model = { session : Session
                   , txt     : HomeSt
                   }

