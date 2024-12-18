module ElmCssMigrationTest exposing (all)

import Css.Migration.OnlyInAttributeList as OnlyInAttributeList
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "all"
        [ onlyInAttributeList
        ]


onlyInAttributeList : Test
onlyInAttributeList =
    describe "OnlyInAttributeList"
        [ test "should not report an error when `css` appears in a list of attributes" <|
            \() ->
                """module A exposing (..)
import Html.Styled as Html
import Html.Styled.Attributes as Attr

a = Html.div [ style ]
        []

style = Attr.css []
"""
                    |> Review.Test.run OnlyInAttributeList.rule
                    |> Review.Test.expectNoErrors
        ]



--         [ test "should not report an error when `css` has a sibling `class`" <|
--             \() ->
--                 """module A exposing (..)
-- import Css
-- import Html.Styled as Html
-- import Html.Styled.Attributes as Attr
-- progressBar =
--     Html.div
--         [ Attr.css
--             [ Css.backgroundColor Colors.greyLighter
--             , Css.borderRadius <| Css.px 9999
--             ]
--         , Attr.class "block"
--         ]
--         []
-- """
--                     |> Review.Test.run rule
--                     |> Review.Test.expectNoErrors
--         , test "should report an error when `css` does not have a sibling `class`" <|
--             \() ->
--                 """module A exposing (..)
-- import Css
-- import Html.Styled as Html
-- import Html.Styled.Attributes as Attr
-- progressBar =
--     Html.div
--         [ Attr.css
--             [ Css.backgroundColor Colors.greyLighter
--             , Css.borderRadius <| Css.px 9999
--             ]
--         ]
--         []
-- """
--                     |> Review.Test.run rule
--                     |> Review.Test.expectErrors
--                         [ Review.Test.error
--                             {}
--                         ]
--         ]
