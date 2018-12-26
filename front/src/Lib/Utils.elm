module Lib.Utils exposing (..)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

applyM = Maybe.map2 (|>)

fmapM = Maybe.map

