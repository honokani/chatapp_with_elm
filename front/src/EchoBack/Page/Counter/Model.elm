module EchoBack.Page.Counter.Model exposing (..)

import EchoBack.Session        exposing (Session, CounterSt)

type alias Model = { session : Session
                   , count   : CounterSt
                   }

