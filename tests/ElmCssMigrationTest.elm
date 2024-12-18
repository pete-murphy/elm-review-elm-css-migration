module ElmCssMigrationTest exposing (all)

import OnlyInAttributeList
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "all"
        [ onlyInAttributeList
        ]


onlyInAttributeListDetails : List String
onlyInAttributeListDetails =
    [ "We first need all instances of `Html.Styled.Attributes.css` to be inside a list of attributes so that we can migrate styles to a sibling `class` attribute."
    ]


onlyInAttributeList : Test
onlyInAttributeList =
    describe "OnlyInAttributeList"
        [ test "should report an error when `css` appears outside a list of attributes" <|
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
        , test "should not report an error when `css` only appears in a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a = Html.div [ Attr.css [] ]
        []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when `css` appears in a list, but it's not a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div [ Attr.css [] ]
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
                            , under = "Attr.css"
                            }
                            |> Review.Test.atExactly { start = { row = 9, column = 17 }, end = { row = 9, column = 25 } }
                        ]
        , test "should report an error when `css` appears unapplied outside of a list" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a =
    Html.div [ css [] ]
        [ let
            style =
                css []
          in
          Html.div [ style ]
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
        ]
