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
import Toasty
import Toasty.Defaults



---- MODEL ----


type alias Model =
    { toasties : Toasty.Stack Toast
    }


type Msg
    = KeyPressed String
    | BtnClicked String
    | ToastyMsg (Toasty.Msg Toast)
    | Animate Int Animation.Msg


type Toast
    = Success ToastBody
    | Warning ToastBody
    | Error ToastBody


type alias ToastBody =
    { title : String
    , body : String
    , animationState : Animation.State
    }


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map KeyPressed <|
        Json.Decode.field "key" Json.Decode.string


init : () -> ( Model, Cmd Msg )
init flags =
    ( { toasties = Toasty.initialState
      }
    , Cmd.none
    )


transitionIn : Toast -> Toast
transitionIn toast =
    case toast of
        Success toastBody ->
            Success
                { toastBody
                    | animationState =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.opacity 1.0
                                ]
                            ]
                            toastBody.animationState
                }

        Warning toastBody ->
            Warning
                { toastBody
                    | animationState =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.opacity 1.0
                                ]
                            ]
                            toastBody.animationState
                }

        Error toastBody ->
            Error
                { toastBody
                    | animationState =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.opacity 1.0
                                ]
                            ]
                            toastBody.animationState
                }


config : Toasty.Config Msg Toast
config =
    Toasty.Defaults.config
        |> Toasty.transitionInFn transitionIn


addToast : Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToast config ToastyMsg toast ( model, cmd )


addToastIfUnique : Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToastIfUnique toast ( model, cmd ) =
    Toasty.addToastIfUnique config ToastyMsg toast ( model, cmd )


addPersistentToast : Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addPersistentToast toast ( model, cmd ) =
    Toasty.addPersistentToast config ToastyMsg toast ( model, cmd )



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
                            (Success
                                { title = "Allright!"
                                , body = "Thing successfully updated"
                                , animationState =
                                    Animation.style
                                        [ Animation.opacity 0.0 ]
                                }
                            )

                "w" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast
                            (Warning
                                { title = "Warning!"
                                , body = "Please check this and that."
                                , animationState =
                                    Animation.style [ Animation.opacity 0.0 ]
                                }
                            )

                "e" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast
                            (Error
                                { title = "Error"
                                , body = "Sorry, something went wrong..."
                                , animationState =
                                    Animation.style [ Animation.opacity 0.0 ]
                                }
                            )

                "p" ->
                    ( model
                    , Cmd.none
                    )
                        |> addPersistentToast
                            (Success
                                { title = "Persistent Toast"
                                , body = "This toast will remain visible until clicked."
                                , animationState =
                                    Animation.style [ Animation.opacity 0.0 ]
                                }
                            )

                "u" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToastIfUnique
                            (Success
                                { title = "Unique toast"
                                , body = "Avoid repeated notifications"
                                , animationState =
                                    Animation.style [ Animation.opacity 0.0 ]
                                }
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        BtnClicked "success" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    (Success
                        { title = "Allright!"
                        , body = "Thing successfully updated"
                        , animationState =
                            Animation.style
                                [ Animation.opacity 0.0 ]
                        }
                    )

        BtnClicked "warning" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    (Warning
                        { title = "Warning!"
                        , body = "Please check this and that."
                        , animationState =
                            Animation.style
                                [ Animation.opacity 0.0 ]
                        }
                    )

        BtnClicked "error" ->
            ( model
            , Cmd.none
            )
                |> addToast
                    (Error
                        { title = "Error"
                        , body = "Sorry, something went wrong..."
                        , animationState =
                            Animation.style
                                [ Animation.opacity 0.0 ]
                        }
                    )

        BtnClicked "persistent" ->
            ( model
            , Cmd.none
            )
                |> addPersistentToast
                    (Success
                        { title = "Persistent Toast"
                        , body = "This toast will remain visible until clicked."
                        , animationState =
                            Animation.style
                                [ Animation.opacity 0.0 ]
                        }
                    )

        BtnClicked "unique" ->
            ( model
            , Cmd.none
            )
                |> addToastIfUnique
                    (Success
                        { title = "Unique toast"
                        , body = "Avoid repeated notifications"
                        , animationState =
                            Animation.style
                                [ Animation.opacity 0.0 ]
                        }
                    )

        BtnClicked _ ->
            ( model
            , Cmd.none
            )

        ToastyMsg subMsg ->
            Toasty.update config ToastyMsg subMsg model

        Animate targetId animMsg ->
            let
                toasts =
                    Toasty.stackToList model.toasties

                newToasts =
                    List.indexedMap
                        (\index ( id, status, toast ) ->
                            if index == targetId then
                                ( id, status, updateToast animMsg toast )

                            else
                                ( id, status, toast )
                        )
                        toasts
            in
            ( { model | toasties = Toasty.listToStack model.toasties newToasts }, Cmd.none )


updateToast : Animation.Msg -> Toast -> Toast
updateToast animMsg toast =
    case toast of
        Success toastBody ->
            Success { toastBody | animationState = Animation.update animMsg toastBody.animationState }

        Warning toastBody ->
            Warning { toastBody | animationState = Animation.update animMsg toastBody.animationState }

        Error toastBody ->
            Error { toastBody | animationState = Animation.update animMsg toastBody.animationState }



-- ( { model
--     | toastAnimation = Animation.update animMsg model.toastAnimation
--   }
-- , Cmd.none
-- )
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


pink : Color
pink =
    rgb255 255 102 178


{-| Default theme view handling the three toast variants.
-}
viewToast : Toast -> Element msg
viewToast toast =
    case toast of
        Success toastBody ->
            genericToast
                ([ Border.rounded 3, Background.color green, padding 10 ]
                    ++ List.map
                        Element.htmlAttribute
                        (Animation.render
                            toastBody.animationState
                        )
                )
                toastBody

        Warning toastBody ->
            genericToast
                ([ Border.rounded 3, Background.color orange, padding 10 ]
                    ++ List.map
                        Element.htmlAttribute
                        (Animation.render
                            toastBody.animationState
                        )
                )
                toastBody

        Error toastBody ->
            genericToast
                ([ Border.rounded 3, Background.color red, padding 10 ]
                    ++ List.map Element.htmlAttribute
                        (Animation.render
                            toastBody.animationState
                        )
                )
                toastBody


genericToast : List (Element.Attribute msg) -> ToastBody -> Element msg
genericToast itemAttributes { title, body } =
    column itemAttributes
        [ el [ Element.Region.heading 1 ] (text title)
        , paragraph [] [ text body ]
        ]


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.inFront (Toasty.view config viewToast ToastyMsg model.toasties)
        ]
        (column
            [ padding 10, spacing 7 ]
            [ el
                [ Element.Region.heading 1
                , Font.extraBold
                ]
                (text "Toasty demo")
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
                , text " or "
                , Input.button
                    [ Border.rounded 3, Background.color pink, padding 3 ]
                    { label = text "unique", onPress = Just (BtnClicked "unique") }
                , text " toast."
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
                , text " for persistent or "
                , el
                    [ Font.family [ Font.monospace ], Font.bold ]
                    (text "[u]")
                , text " for unique toasts."
                ]
            , paragraph [] [ text "Click on any toast to remove it." ]
            , paragraph [] [ text "This demo uses ", el [] (text "Toasty.Defaults for styling.") ]
            , paragraph []
                [ link []
                    { url = "http://package.elm-lang.org/packages/pablen/toasty/latest"
                    , label = text "Toasty at package.elm-lang.org"
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
        (List.indexedMap
            animationSubscriber
            (Toasty.stackToList model.toasties)
            ++ [ Browser.Events.onKeyPress keyDecoder ]
        )


animationSubscriber : Int -> ( Int, a, Toast ) -> Sub Msg
animationSubscriber index ( id, _, toast ) =
    Animation.subscription (Animate index) [ stateGrabber toast ]


stateGrabber : Toast -> Animation.State
stateGrabber toast =
    case toast of
        Success toastBody ->
            toastBody.animationState

        Warning toastBody ->
            toastBody.animationState

        Error toastBody ->
            toastBody.animationState



-- \_ -> Browser.Events.onKeyPress keyDecoder
