module Config exposing (all)

import Expect
import Html exposing (..)
import Html.Attributes exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Toast


type alias Toast =
    String


type Msg
    = Tagger (Toast.Msg Toast)


initialModel : { toasties : Toast.Stack Toast }
initialModel =
    { toasties = Toast.initialState }


renderToast : Toast -> Html Msg
renderToast toast =
    div [] [ text toast ]


all : Test
all =
    describe "HTML rendering"
        [ test "Initially renders an empty text node" <|
            \() ->
                let
                    view =
                        Toast.view Toast.config renderToast Tagger initialModel.toasties
                in
                Expect.equal view (text "")
        , test "Renders list of toasts after adding toasts" <|
            \() ->
                let
                    ( model, cmd ) =
                        ( initialModel
                        , Cmd.none
                        )
                            |> Toast.addToast Toast.config Tagger "foo"
                            |> Toast.addToast Toast.config Tagger "bar"

                    view =
                        Toast.view Toast.config renderToast Tagger model.toasties
                in
                view
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Expect.all
                        [ Query.index 0 >> Query.has [ Selector.text "foo" ]
                        , Query.index 1 >> Query.has [ Selector.text "bar" ]
                        ]
        , test "Can add custom attributes to list container" <|
            \() ->
                let
                    ( model, cmd ) =
                        ( initialModel
                        , Cmd.none
                        )
                            |> Toast.addToast Toast.config Tagger "foo"

                    myConfig =
                        Toast.config
                            |> Toast.containerAttrs [ class "myClass", style "color" "red" ]

                    view =
                        Toast.view myConfig renderToast Tagger model.toasties
                in
                -- elm-test can't test style attributess ATM https://github.com/eeue56/elm-html-test/issues/3
                view
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "myClass" ]
        , test "Can add custom attributes to toast container" <|
            \() ->
                let
                    ( model, cmd ) =
                        ( initialModel
                        , Cmd.none
                        )
                            |> Toast.addToast Toast.config Tagger "foo"

                    myConfig =
                        Toast.config
                            |> Toast.itemAttrs [ class "itemClass", style "color" "blue" ]

                    view =
                        Toast.view myConfig renderToast Tagger model.toasties
                in
                -- elm-test can't test style attributess ATM https://github.com/eeue56/elm-html-test/issues/3
                view
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Query.each (Query.has [ Selector.class "itemClass" ])
        , test "Can add custom attributes to toast container when transitioning in" <|
            \() ->
                let
                    ( model, cmd ) =
                        ( initialModel
                        , Cmd.none
                        )
                            |> Toast.addToast Toast.config Tagger "foo"

                    myConfig =
                        Toast.config
                            |> Toast.transitionInAttrs [ class "fadeIn", style "color" "green" ]

                    view =
                        Toast.view myConfig renderToast Tagger model.toasties
                in
                -- elm-test can't test style attributess ATM https://github.com/eeue56/elm-html-test/issues/3
                view
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Query.each (Query.has [ Selector.class "fadeIn" ])
        ]
