module App exposing (main)

import Browser
import Browser.Events
import Char
import Element exposing (..)
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
    { toasties : Toasty.Stack Toasty.Defaults.Toast
    }


type Msg
    = KeyPressed String
    | BtnClicked String
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map KeyPressed <|
        Json.Decode.field "key" Json.Decode.string


init : () -> ( Model, Cmd Msg )
init flags =
    ( { toasties = Toasty.initialState }
    , Cmd.none
    )


myConfig : Toasty.Config Msg
myConfig =
    Toasty.Defaults.config
        |> Toasty.delay 5000


addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToast myConfig ToastyMsg toast ( model, cmd )


addToastIfUnique : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToastIfUnique toast ( model, cmd ) =
    Toasty.addToastIfUnique myConfig ToastyMsg toast ( model, cmd )


addPersistentToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addPersistentToast toast ( model, cmd ) =
    Toasty.addPersistentToast myConfig ToastyMsg toast ( model, cmd )



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
                        |> addToast (Toasty.Defaults.Success "Allright!" "Thing successfully updated")

                "w" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast (Toasty.Defaults.Warning "Warning!" "Please check this and that.")

                "e" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToast (Toasty.Defaults.Error "Error" "Sorry, something went wrong...")

                "p" ->
                    ( model
                    , Cmd.none
                    )
                        |> addPersistentToast (Toasty.Defaults.Success "Persistent Toast" "This toast will remain visible until clicked.")

                "u" ->
                    ( model
                    , Cmd.none
                    )
                        |> addToastIfUnique (Toasty.Defaults.Success "Unique toast" "Avoid repeated notifications")

                _ ->
                    ( model
                    , Cmd.none
                    )

        BtnClicked "success" ->
            ( model
            , Cmd.none
            )
                |> addToast (Toasty.Defaults.Success "Allright!" "Thing successfully updated")

        BtnClicked "warning" ->
            ( model
            , Cmd.none
            )
                |> addToast (Toasty.Defaults.Warning "Warning!" "Please check this and that.")

        BtnClicked "error" ->
            ( model
            , Cmd.none
            )
                |> addToast (Toasty.Defaults.Error "Error" "Sorry, something went wrong...")

        BtnClicked "persistent" ->
            ( model
            , Cmd.none
            )
                |> addPersistentToast (Toasty.Defaults.Success "Persistent Toast" "This toast will remain visible until clicked.")

        BtnClicked "unique" ->
            ( model
            , Cmd.none
            )
                |> addToastIfUnique (Toasty.Defaults.Success "Unique toast" "Avoid repeated notifications")

        BtnClicked _ ->
            ( model
            , Cmd.none
            )

        ToastyMsg subMsg ->
            Toasty.update myConfig ToastyMsg subMsg model



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Element.layout []
        (column
            []
            [ el [ Element.Region.heading 1 ] (text "Toasty demo")
            , paragraph []
                [ text "Click for adding a "
                , Input.button [] { label = text "success", onPress = Just (BtnClicked "success") }
                , text ", "
                , Input.button [] { label = text "warning", onPress = Just (BtnClicked "warning") }
                , text ", "
                , Input.button [] { label = text "error", onPress = Just (BtnClicked "error") }
                , text ", "
                , Input.button [] { label = text "persistent", onPress = Just (BtnClicked "persistent") }
                , text " or "
                , Input.button [] { label = text "unique", onPress = Just (BtnClicked "unique") }
                , text " toast."
                ]
            , paragraph []
                [ text "Also you can press in your keyboard "
                , el [] (text "[s]")
                , text " for success, "
                , el [] (text "[w]")
                , text " for warning, "
                , el [] (text "[e]")
                , text " for error, "
                , el [] (text "[p]")
                , text " for persistent or "
                , el [] (text "[u]")
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
            , Toasty.view myConfig Toasty.Defaults.view ToastyMsg model.toasties
            ]
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Browser.Events.onKeyPress keyDecoder
        }
