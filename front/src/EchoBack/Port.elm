port module EchoBack.Port exposing (..)

-- in
-- port rcvState  : (String       -> msg) -> Sub msg
port checkSign : (Maybe String -> msg) -> Sub msg

-- out
port signIn   : ()     -> Cmd msg 
port signOut  : ()     -> Cmd msg 

