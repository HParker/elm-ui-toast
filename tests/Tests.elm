module Tests exposing (all)

import Config
import Test exposing (..)


all : Test
all =
    describe "Toast Test Suite"
        [ Config.all
        ]
