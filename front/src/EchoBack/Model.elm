module EchoBack.Model exposing (..)

import EchoBack.Auth.Model         as Auth exposing (..)
import EchoBack.Page.Home.Model    as Home exposing (..)
import EchoBack.Page.Chat.Model    as Chat exposing (..)
import EchoBack.Page.Counter.Model as Cntr exposing (..)

type Model = MdlHome Home.Model
           | MdlCntr Cntr.Model
           | MdlChat Chat.Model

