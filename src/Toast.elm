module Toast exposing
    ( Stack, Msg
    , config, delay, transitionOutDuration, containerAttrs, Config
    , view, update, addToast, addPersistentToast, initialState
    , subscription, transitionInFn, transitionOutFn
    )

{-| This package lets you easily show customizable toast notifications in your
Elm apps following The Elm Architecture. You will be able to trigger toasts as
a side-effect of model updates by piping your update function return value
through this library `addToast` function.

While this package lets you configure each part of the rendering and behaviour
of the notification stack, you can use a nice default theme configuration provided
in `Toast.Defaults`.


## Example


### Setting things up

To use the package, let's look at an example that shows a simple text notification.

First you add the toast stack to your model, wrapping the toast model you want in `Stack`.
You must do it in a field called `toasties`:

    type alias Model =
        { toasties : Toast.Stack String }

Note that in this example we are modelling our toasts as a simple `String`,
but we can model our toasts however we need.

Add the stack initial state in your `init` function:

    init : ( Model, Cmd Msg )
    init =
        ( { toasties = Toast.initialState }, Cmd.none )

Then add a message that will handle toasts messages:

    type alias Msg =
        ToastMsg (Toast.Msg String)

You can use the default configuration as-is or tweak it to your needs by piping configuration helpers:

    myConfig : Toast.Config msg
    myConfig =
        Toast.config
            |> Toast.transitionOutDuration 100
            |> Toast.delay 8000

Handle the toasts message in your app update function using the library `update`
function:

    update msg model =
        case msg of
            ToastMsg subMsg ->
                Toast.update myConfig ToastMsg subMsg model

As a last step, render the toast stack in you `view` function. You will need to
provide a special view function that knows how to render your toast model:

    view : Model -> Html Msg
    view model =
        div []
            [ h1 [] [ text "Toast example" ]
            , Toast.view myConfig renderToast ToastMsg model.toasties
            ]

    renderToast : String -> Html Msg
    renderToast toast =
        div [] [ text toast ]


### Triggering toasts

A pretty common scenario is to trigger toasts as side-effect of some other app event,
e.g. show a message when an asynchronous response was received. In order to do that, just
pipe your update function return value through the `addToast` function passing your
configuration, tag and toast.

        update msg model =
            case msg of
                SomeAppMsg ->
                    ( newModel, Cmd.none )
                        |> Toast.addToast myConfig ToastMsg "Entity successfully created!"

That's all!


# Definition

@docs Stack, Msg


# Configuration

The notifications appearance and behaviour can be fully customized. To do this,
you need to import the default configuration and tweak it by piping the provided
helper functions.

Note that as you can set container and items HTML attributes the library remains
agnostic about how to style your toasts, enabling you to use inline styles or
classes.

    myConfig : Toast.Config msg
    myConfig =
        Toast.config
            |> Toast.transitionOutDuration 700
            |> Toast.delay 8000
            |> Toast.containerAttrs containerAttrs

    containerAttrs =
        [ style "max-width" "300px"
        , style "position" "fixed"
        , style "right" "0"
        , style "top" "0"
        ]

@docs config, delay, transitionOutDuration, containerAttrs, Config


# Other functions

@docs view, update, addToast, addPersistentToast, addToastIf, addToastIfUnique, initialState

-}

import Animation
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Keyed
import Process
import Random exposing (Seed)
import Task


{-| Represents the stack of current toasts notifications. You can model a toast
to be as complex or simple as you want.

    type alias Model =
        { toasties : Toast.Stack MyToast
        }

    -- Defines a toast model that has three different variants
    type MyToast
        = Success String
        | Warning String
        | Error String String

-}
type Stack a
    = Stack (List ( Id, Status, { a | animationState : Animation.State } )) Seed


{-| How the toast will be removed.

Temporary toasts are removed after a timeout or after a click,
Persistent toasts must be clicked to be removed.

-}
type RemoveBehaviour
    = Temporary
    | Persistent


{-| The internal message type used by the library. You need to tag and add it to your app messages.

    type Msg
        = ToastMsg (Toast.Msg MyToast)

-}
type Msg a
    = Add { a | animationState : Animation.State }
    | Remove Id
    | TransitionOut Id
    | Animate Int Animation.Msg


{-| The base configuration type.
-}
type Config msg a
    = Config
        { transitionOutDuration : Float
        , containerAttrs : List (Element.Attribute msg)
        , transitionInFn : { a | animationState : Animation.State } -> { a | animationState : Animation.State }
        , transitionOutFn : { a | animationState : Animation.State } -> { a | animationState : Animation.State }
        , delay : Float
        }


type alias Id =
    Int


type Status
    = Entered
    | Leaving


type alias Toasted m a =
    { m | toasties : Stack a }


{-| Some basic configuration defaults: Toasts are visible for 5 seconds with
no animations or special styling.
-}
config : Config msg a
config =
    Config
        { transitionOutDuration = 0
        , containerAttrs = []
        , transitionInFn = \a -> a
        , transitionOutFn = \a -> a
        , delay = 5000
        }


{-| Changes the amount of time (in milliseconds) to wait after transition out
begins and before actually removing the toast node from the DOM. This lets you
author fancy animations when a toast is removed.
-}
transitionOutDuration : Float -> Config msg a -> Config msg a
transitionOutDuration time (Config cfg) =
    Config { cfg | transitionOutDuration = time }


transitionInFn : ({ a | animationState : Animation.State } -> { a | animationState : Animation.State }) -> Config msg a -> Config msg a
transitionInFn func (Config cfg) =
    Config { cfg | transitionInFn = func }


transitionOutFn : ({ a | animationState : Animation.State } -> { a | animationState : Animation.State }) -> Config msg a -> Config msg a
transitionOutFn func (Config cfg) =
    Config { cfg | transitionOutFn = func }


{-| Lets you set the HTML attributes to add to the toasts stack container. This will help
you style and position the toast stack however you like by adding classes or inline styles.
-}
containerAttrs : List (Element.Attribute msg) -> Config msg a -> Config msg a
containerAttrs attrs (Config cfg) =
    Config { cfg | containerAttrs = attrs }


{-| Changes the amount of time (in milliseconds) the toast will be visible.
After this time, the transition out begins.
-}
delay : Float -> Config msg a -> Config msg a
delay time (Config cfg) =
    Config { cfg | delay = time }


{-| An empty stack of toasts to initialize your model with.
-}
initialState : Stack a
initialState =
    Stack [] (Random.initialSeed 0)


toastExtractor : ( Id, Status, a ) -> a
toastExtractor ( id, status, toast ) =
    toast


{-| Handles the internal messages. You need to wire it to your app update function

    update msg model =
        case msg of
            ToastMsg subMsg ->
                Toast.update Toast.config ToastMsg subMsg model

-}
update : Config msg a -> (Msg a -> msg) -> Msg a -> Toasted m a -> ( Toasted m a, Cmd msg )
update (Config cfg) tagger msg model =
    let
        (Stack toasts seed) =
            model.toasties
    in
    case msg of
        Add toast ->
            addToast config tagger toast ( model, Cmd.none )

        Remove targetId ->
            let
                newStack : List ( Id, Status, { a | animationState : Animation.State } )
                newStack =
                    List.filter (\( id, toast, status ) -> id /= targetId) toasts
            in
            ( { model
                | toasties = Stack newStack seed
              }
            , Cmd.none
            )

        TransitionOut targetId ->
            let
                newStack =
                    List.map
                        (\( id, status, toast ) ->
                            if id == targetId then
                                ( id, Leaving, cfg.transitionOutFn toast )

                            else
                                ( id, status, toast )
                        )
                        toasts
            in
            ( { model
                | toasties = Stack newStack seed
              }
            , Task.perform (\_ -> tagger (Remove targetId)) (Process.sleep <| cfg.transitionOutDuration)
            )

        Animate targetIndex animMsg ->
            let
                newToasts =
                    List.indexedMap
                        (\index ( id, status, toast ) ->
                            if index == targetIndex then
                                ( id, status, animateToast animMsg toast )

                            else
                                ( id, status, toast )
                        )
                        toasts
            in
            ( { model | toasties = Stack newToasts seed }, Cmd.none )


animateToast : Animation.Msg -> { a | animationState : Animation.State } -> { a | animationState : Animation.State }
animateToast animMsg toast =
    { toast | animationState = Animation.update animMsg toast.animationState }


{-| Adds a toast to the stack and schedules its removal. It receives and returns
a tuple of type '(model, Cmd msg)' so that you can easily pipe it to your app
update function branches.

    update msg model =
        case msg of
            SomeAppMsg ->
                ( newModel, Cmd.none )
                    |> Toast.addToast myConfig ToastMsg (MyToast "Entity successfully created!")

            ToastMsg subMsg ->
                Toast.update myConfig ToastMsg subMsg model

-}
addToast : Config msg a -> (Msg a -> msg) -> { a | animationState : Animation.State } -> ( Toasted m a, Cmd msg ) -> ( Toasted m a, Cmd msg )
addToast =
    addToast_ Temporary


{-| Similar to `addToast` but doesn't schedule the toast removal, so it will remain visible until clicked.
-}
addPersistentToast : Config msg a -> (Msg a -> msg) -> { a | animationState : Animation.State } -> ( Toasted m a, Cmd msg ) -> ( Toasted m a, Cmd msg )
addPersistentToast =
    addToast_ Persistent


{-| Figure out whether a stack contains a specific toast. Similar to `List.member`.
-}



-- hasToast : a -> Stack a -> Bool
-- hasToast toast (Stack toasts _) =
--     toasts
--         |> List.map (\( _, _, t ) -> t)
--         |> List.member toast


addToast_ : RemoveBehaviour -> Config msg a -> (Msg a -> msg) -> { a | animationState : Animation.State } -> ( Toasted m a, Cmd msg ) -> ( Toasted m a, Cmd msg )
addToast_ removeBehaviour (Config cfg) tagger toast ( model, cmd ) =
    let
        (Stack toasts seed) =
            model.toasties

        ( newId, newSeed ) =
            getNewId seed

        task =
            case removeBehaviour of
                Temporary ->
                    Task.perform (\() -> tagger (TransitionOut newId)) (Process.sleep <| cfg.delay)

                Persistent ->
                    Cmd.none
    in
    ( { model
        | toasties =
            Stack
                (toasts
                    ++ [ ( newId
                         , Entered
                         , cfg.transitionInFn toast
                         )
                       ]
                )
                newSeed
      }
    , Cmd.batch [ cmd, task ]
    )


{-| Renders the stack of toasts. You need to add it to your app view function and
give it a function that knows how to render your toasts model.

    view model =
        div []
            [ h1 [] [ text "Toast example" ]
            , Toast.view myConfig (\txt -> div [] [ text txt ]) ToastMsg model.toasties
            ]

-}
view : Config msg a -> ({ a | animationState : Animation.State } -> Element msg) -> (Msg a -> msg) -> Stack a -> Element msg
view cfg toastView tagger (Stack toasts seed) =
    let
        (Config c) =
            cfg
    in
    if List.isEmpty toasts then
        text ""

    else
        Element.Keyed.column c.containerAttrs <| List.map (\toast -> itemContainer cfg tagger toast toastView) toasts


getNewId : Seed -> ( Id, Seed )
getNewId seed =
    Random.step (Random.int Random.minInt Random.maxInt) seed


itemContainer : Config msg a -> (Msg a -> msg) -> ( Id, Status, { a | animationState : Animation.State } ) -> ({ a | animationState : Animation.State } -> Element msg) -> ( String, Element msg )
itemContainer (Config cfg) tagger ( id, status, toast ) toastView =
    ( String.fromInt id, el [ onClick (tagger <| TransitionOut id) ] (toastView toast) )



-- SUBSCRIPTIONS


subscription : (Msg a -> msg) -> Toasted m a -> Sub msg
subscription tagger model =
    let
        (Stack toasts seed) =
            model.toasties
    in
    Sub.batch
        (List.indexedMap
            (animationSubscriber tagger)
            toasts
        )


animationSubscriber : (Msg a -> msg) -> Int -> ( Int, s, { a | animationState : Animation.State } ) -> Sub msg
animationSubscriber tagger index ( id, _, toast ) =
    Animation.subscription (Animate index >> tagger) [ toast.animationState ]
