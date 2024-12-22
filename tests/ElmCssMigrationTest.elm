module ElmCssMigrationTest exposing (all)

import Expect
import OnlyInAttributeList
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "all"
        [ onlyInAttributeList
        ]


onlyInAttributeListDetails : List String
onlyInAttributeListDetails =
    [ "We first need all instances of `Html.Styled.Attributes.css` to be inside a list of attributes so that we can migrate styles to a sibling `class` attribute."
    ]


onlyInAttributeList : Test
onlyInAttributeList =
    Test.describe "OnlyInAttributeList" <|
        [ Test.test "should report an error when `css` appears outside a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a = Html.div [ style ]
        []

style = Attr.css []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found `css` attribute outside of list"
                            , details = onlyInAttributeListDetails
                            , under = "Attr.css"
                            }
                        ]
        , Test.test "should not report an error when `css` only appears in a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a = Html.div [ Attr.css [] ]
        []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectNoErrors
        , Test.test "should not report an error when `css` appears twice in a list of attributes" <|
            \() ->
                """module A exposing (..)

import Html.Styled as Html
import Html.Styled.Attributes as Attr


a =
    Html.div
        [ Attr.css []
        , Attr.css []
        ]
        []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectNoErrors
        , Test.test "should report an error when `css` appears in a list, but it's not a list of attributes, fix should apply to all instances" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div []
        [ let
            style =
                Attr.css []
          in
          Html.div [ if True then style else style ]
            [ Html.text "Hello, World!" ]
        ]
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found `css` attribute outside of list"
                            , details = onlyInAttributeListDetails
                            , under = """let
            style =
                Attr.css []
          in
          Html.div [ if True then style else style ]
            [ Html.text "Hello, World!" ]"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div []
        [ Html.div [ if True then Attr.css [] else Attr.css [] ]
            [ Html.text "Hello, World!" ]
        ]
"""
                        ]
        , Test.test "should report an error when `css` appears in a list, but it's not a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div []
        [ let
            style =
                Attr.css []
          in
          Html.div [ style ]
            [ Html.text "Hello, World!" ]
        ]
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found `css` attribute outside of list"
                            , details = onlyInAttributeListDetails
                            , under = """let
            style =
                Attr.css []
          in
          Html.div [ style ]
            [ Html.text "Hello, World!" ]"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div []
        [ Html.div [ Attr.css [] ]
            [ Html.text "Hello, World!" ]
        ]
"""
                        ]
        , Test.test "should report an error when `css` appears unapplied outside of a list" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div [ css [] ]
        [ Html.div [ ]
            [ Html.text "Hello, World!" ]
        ]

css = Attr.css
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found `css` attribute outside of list"
                            , details = onlyInAttributeListDetails
                            , under = "Attr.css"
                            }
                        ]
        , Test.test "should not report an error when `css` appears as argument to cons (::) operator" <|
            \() ->
                """module A exposing (..)

import Html.Styled as Html
import Html.Styled.Attributes as Attr


a =
    Html.div
        (Attr.css []
            :: otherAttrs
        )
        []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectNoErrors
        ]
