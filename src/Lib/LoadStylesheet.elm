module Lib.LoadStylesheet exposing (stylesheetDiv)
-- common modules
import Html               exposing (Html, div, node)
import Html.Attributes    exposing (attribute, class)


styleFilePath : String
styleFilePath = "main.css"


-- stylesheet setting for elm-reactor
loadStylesheet : Html msg
loadStylesheet =
    let attrs = [ attribute "rel"      "stylesheet"
                , attribute "property" "stylesheet"
                , attribute "href"     styleFilePath
                ]
    in node "link" attrs []


stylesheetDiv : Html msg
stylesheetDiv = div [class "stylesheet"] [loadStylesheet]

