module Toasty.Defaults exposing (config)

{-| This module provides a generic toast type with three variants (success, error and warning)
each one supports a title and optional secondary text.

**You need to load the provided `Defaults.css` file in your project**. `bounceInRight`
and `fadeOutRightBig` animations borrowed from [Animate.css](https://daneden.github.io/animate.css/)
project by Daniel Eden.

See a [demo](http://pablen-toasty-demo.surge.sh/).


# Definition

@docs Toast, config, view

-}

import Animation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Region
import Html
import Html.Attributes
import Toasty


{-| Default theme configuration.
-}
config : Toasty.Config msg toast
config =
    Toasty.config
        |> Toasty.transitionOutDuration 600
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.transitionInFn transitionIn
        |> Toasty.transitionOutFn transitionOut
        |> Toasty.delay 5000


transitionIn : { toast | animationState : Animation.State } -> { toast | animationState : Animation.State }
transitionIn toast =
    { toast
        | animationState =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 1.0
                    ]
                ]
                toast.animationState
    }


transitionOut : { toast | animationState : Animation.State } -> { toast | animationState : Animation.State }
transitionOut toast =
    { toast
        | animationState =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 0.0
                    ]
                ]
                toast.animationState
    }


containerAttrs : List (Element.Attribute msg)
containerAttrs =
    [ Element.alignRight
    , Element.pointer
    , Element.width (Element.px 300)
    , padding 10
    , spacing 10
    ]
