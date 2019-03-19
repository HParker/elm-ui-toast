module App exposing (main)

import Animation
import Browser
import Browser.Events
import Char
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region
import Html
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode
import Toast
import Toast.Defaults



---- MODEL ----


type alias Model =
    { toasties : Toast.Stack Toast
    }


type Msg
    = KeyPressed String
    | BtnClicked String
    | ToastMsg (Toast.Msg Toast)


type ToastType
    = Success
    | Warning
    | Error
    | Persistent


type alias Toast =
    { title : String
    , body : String
    , animationState : Animation.State
    , toastType : ToastType
    }


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map KeyPressed <|
        Json.Decode.field "key" Json.Decode.string


init : () -> ( Model, Cmd Msg )
init flags =
    ( { toasties = Toast.initialState
      }
    , Cmd.none
    )


transitionIn : Toast -> Toast
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


transitionOut : Toast -> Toast
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


config : Toast.Config Msg Toast
config =
    Toast.Defaults.config


addToast : Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toast.addToast config ToastMsg toast ( model, cmd )


addPersistentToast : Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addPersistentToast toast ( model, cmd ) =
    Toast.addPersistentToast config ToastMsg toast ( model, cmd )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed keycode ->
            case keycode of
                "s" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast
                            { title = "Allright!"
                            , body = "Thing successfully updated"
                            , toastType = Success
                            , animationState =
                                Animation.style
                                    [ Animation.opacity 0.0 ]
                            }

                "w" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast
                            { title = "Warning!"
                            , body = "Please check this and that."
                            , toastType = Warning
                            , animationState =
                                Animation.style [ Animation.opacity 0.0 ]
                            }

                "e" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast
                            { title = "Error"
                            , body = "Sorry, something went wrong..."
                            , toastType = Error
                            , animationState =
                                Animation.style [ Animation.opacity 0.0 ]
                            }

                "p" ->
                    ( model
                    , Cmd.none
                    )
                        |> addPersistentToast
                            { title = "Persistent Toast"
                            , body = "This toast will remain visible until clicked."
                            , toastType = Persistent
                            , animationState =
                                Animation.style [ Animation.opacity 0.0 ]
                            }

                _ ->
                    ( model
                    , Cmd.none
                    )

        BtnClicked "success" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    { title = "Allright!"
                    , body = "Thing successfully updated"
                    , toastType = Success
                    , animationState =
                        Animation.style
                            [ Animation.opacity 0.0 ]
                    }

        BtnClicked "warning" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    { title = "Warning!"
                    , body = "Please check this and that."
                    , toastType = Warning
                    , animationState =
                        Animation.style
                            [ Animation.opacity 0.0 ]
                    }

        BtnClicked "error" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    { title = "Error"
                    , body = "Sorry, something went wrong..."
                    , toastType = Error
                    , animationState =
                        Animation.style
                            [ Animation.opacity 0.0 ]
                    }

        BtnClicked "persistent" ->
            ( model
            , Cmd.none
            )
                |> addPersistentToast
                    { title = "Persistent Toast"
                    , body = "This toast will remain visible until clicked."
                    , toastType = Persistent
                    , animationState =
                        Animation.style
                            [ Animation.opacity 0.0 ]
                    }

        BtnClicked _ ->
            ( model
            , Cmd.none
            )

        ToastMsg subMsg ->
            Toast.update config ToastMsg subMsg model



---- VIEW ----


red : Color
red =
    rgb255 255 51 51


orange : Color
orange =
    rgb255 255 153 51


green : Color
green =
    rgb255 153 255 51


purple : Color
purple =
    rgb255 153 51 255


{-| Default theme view handling the three toast variants.
-}
viewToast : Toast -> Element msg
viewToast toast =
    case toast.toastType of
        Success ->
            genericToast
                ([ Border.rounded 3, Background.color green, padding 10 ]
                    ++ List.map
                        Element.htmlAttribute
                        (Animation.render
                            toast.animationState
                        )
                )
                toast

        Warning ->
            genericToast
                ([ Border.rounded 3, Background.color orange, padding 10 ]
                    ++ List.map
                        Element.htmlAttribute
                        (Animation.render
                            toast.animationState
                        )
                )
                toast

        Error ->
            genericToast
                ([ Border.rounded 3, Background.color red, padding 10 ]
                    ++ List.map Element.htmlAttribute
                        (Animation.render
                            toast.animationState
                        )
                )
                toast

        Persistent ->
            genericToast
                ([ Border.rounded 3, Background.color purple, padding 10 ]
                    ++ List.map Element.htmlAttribute
                        (Animation.render
                            toast.animationState
                        )
                )
                toast


genericToast : List (Element.Attribute msg) -> Toast -> Element msg
genericToast itemAttributes { title, body } =
    column itemAttributes
        [ el [ Element.Region.heading 1 ] (text title)
        , paragraph [] [ text body ]
        ]


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.inFront (Toast.view config viewToast ToastMsg model.toasties)
        ]
        (column
            [ padding 10, spacing 7 ]
            [ el
                [ Element.Region.heading 1
                , Font.extraBold
                ]
                (text "Toast demo")
            , paragraph []
                [ text "Click for adding a "
                , Input.button
                    [ Border.rounded 3, Background.color green, padding 3 ]
                    { label = text "success", onPress = Just (BtnClicked "success") }
                , text ", "
                , Input.button
                    [ Border.rounded 3, Background.color orange, padding 3 ]
                    { label = text "warning", onPress = Just (BtnClicked "warning") }
                , text ", "
                , Input.button
                    [ Border.rounded 3, Background.color red, padding 3 ]
                    { label = text "error", onPress = Just (BtnClicked "error") }
                , text ", "
                , Input.button
                    [ Border.rounded 3, Background.color purple, padding 3 ]
                    { label = text "persistent", onPress = Just (BtnClicked "persistent") }
                ]
            , paragraph []
                [ text "Also you can press in your keyboard "
                , el
                    [ Font.family [ Font.monospace ], Font.bold ]
                    (text "[s]")
                , text " for success, "
                , el
                    [ Font.family [ Font.monospace ], Font.bold ]
                    (text "[w]")
                , text " for warning, "
                , el
                    [ Font.family [ Font.monospace ], Font.bold ]
                    (text "[e]")
                , text " for error, "
                , el
                    [ Font.family [ Font.monospace ], Font.bold ]
                    (text "[p]")
                , text " for persistent."
                ]
            , paragraph [] [ text "Click on any toast to remove it." ]
            , paragraph []
                [ link []
                    { url = "http://package.elm-lang.org/packages/pablen/toasty/latest"
                    , label = text "Toast at package.elm-lang.org"
                    }
                ]
            ]
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress keyDecoder
        , Toast.subscription ToastMsg model
        ]
