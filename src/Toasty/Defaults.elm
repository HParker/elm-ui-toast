module Toasty.Defaults exposing (Toast(..), config, view)

{-| This module provides a generic toast type with three variants (success, error and warning)
each one supports a title and optional secondary text.

**You need to load the provided `Defaults.css` file in your project**. `bounceInRight`
and `fadeOutRightBig` animations borrowed from [Animate.css](https://daneden.github.io/animate.css/)
project by Daniel Eden.

See a [demo](http://pablen-toasty-demo.surge.sh/).


# Definition

@docs Toast, config, view

-}

import Element exposing (..)
import Element.Region
import Html
import Html.Attributes
import Toasty


{-| This theme defines toasts of three variants: "Success", "Warning" and "Error".
Each of them accepts a title and an optional secondary text.
-}
type Toast
    = Success String String
    | Warning String String
    | Error String String


{-| Default theme configuration.
-}
config : Toasty.Config msg
config =
    Toasty.config
        |> Toasty.transitionOutDuration 700
        |> Toasty.transitionOutAttrs transitionOutAttrs
        |> Toasty.transitionInAttrs transitionInAttrs
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs
        |> Toasty.delay 5000


containerAttrs : List (Element.Attribute msg)
containerAttrs =
    [ Element.centerX
    , Element.pointer
    , Element.width (Element.px 300)
    ]


itemAttrs : List (Element.Attribute msg)
itemAttrs =
    []


transitionInAttrs : List (Element.Attribute msg)
transitionInAttrs =
    []


transitionOutAttrs : List (Element.Attribute msg)
transitionOutAttrs =
    []


{-| Default theme view handling the three toast variants.
-}
view : Toast -> Element msg
view toast =
    case toast of
        Success title message ->
            genericToast "toasty-success" title message

        Warning title message ->
            genericToast "toasty-warning" title message

        Error title message ->
            genericToast "toasty-error" title message


genericToast : String -> String -> String -> Element msg
genericToast variantClass title message =
    row
        []
        [ el [ Element.Region.heading 1 ] (text title)
        , if String.isEmpty message then
            text ""

          else
            paragraph [] [ text message ]
        ]
