module DebugVerbose where

import Debug.Trace (trace)

verbose :: Bool -> String -> a -> a
verbose v msg x =
    if v 
    then trace msg x
    else x

state :: Bool -> String -> a -> a
state v msg x = 
    if v 
    then trace msg x 
    else x
