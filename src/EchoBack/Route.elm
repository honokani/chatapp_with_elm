module EchoBack.Route exposing (..)
-- common modules
import Url                  exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)

type Route = Root
           | Home
           | Counter
           | Chat


urlParser : Parser (Route -> a) a
urlParser = oneOf [ Parser.map Root    Parser.top
                  , Parser.map Home    (s "home")
                  , Parser.map Counter (s "counter")
                  , Parser.map Chat    (s "chat")
                  ]


fromUrl : Url -> Maybe Route
fromUrl u = { u | path = Maybe.withDefault "" u.fragment, fragment = Nothing }
            |> Parser.parse urlParser


-- Internal
routeToString : Route -> String
routeToString page =
    let
        pieces = case page of
            Root    -> []
            Home    -> []
            Counter -> ["counter"]
            Chat    -> ["chat"]
    in
        "#/" ++ String.join "/" pieces

