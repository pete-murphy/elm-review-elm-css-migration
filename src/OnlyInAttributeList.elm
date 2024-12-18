module OnlyInAttributeList exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ OnlyInAttributeList.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template pete-murphy/elm-review-elm-css-migration/example --rules OnlyInAttributeList
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Css.Migration.OnlyInAttributeList" []
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        -- |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    Rule.ModuleRuleSchema a ModuleContext
    -> Rule.ModuleRuleSchema { a | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator (\x -> x.acc)


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts x y =
    x ++ y


type alias AccItem =
    { moduleName : ModuleName
    , range : Range
    , sourceCode : String
    , nonCssRanges : List Range
    , simpleRanges : List Range
    , countTotal : Int
    }


type alias ProjectContext =
    List AccItem


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable key name extract projectContext ->
            { lookupTable = lookupTable
            , moduleKey = key
            , moduleName = name
            , sourceCodeExtractor = extract
            , acc = projectContext
            , ignoredNodes = []
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleKey
        |> Rule.withModuleName
        |> Rule.withSourceCodeExtractor


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleKey : Rule.ModuleKey
    , moduleName : ModuleName
    , sourceCodeExtractor : Range -> String
    , acc : List AccItem
    , ignoredNodes : List (Node Expression)
    }


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    if List.member node context.ignoredNodes then
        case node of
            Node _ (Expression.Application (((Node _ (Expression.FunctionOrValue _ "css")) as cssNode) :: _)) ->
                case ModuleNameLookupTable.moduleNameFor context.lookupTable cssNode of
                    Just [ "Html", "Styled", "Attributes" ] ->
                        ( [], { context | ignoredNodes = cssNode :: context.ignoredNodes } )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

    else
        case node of
            Node _ (Expression.ListExpr nodes) ->
                ( [], { context | ignoredNodes = nodes } )

            Node range (Expression.FunctionOrValue _ "css") ->
                case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                    Just [ "Html", "Styled", "Attributes" ] ->
                        ( [ Rule.errorForModule context.moduleKey
                                { message = "Found `css` attribute outside of list"
                                , details =
                                    [ "We first need all instances of `Html.Styled.Attributes.css` to be inside a list of attributes so that we can migrate styles to a sibling `class` attribute."
                                    ]
                                }
                                range
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

            Node _ (Expression.Application (((Node range (Expression.FunctionOrValue _ "css")) as cssNode) :: _)) ->
                case ModuleNameLookupTable.moduleNameFor context.lookupTable cssNode of
                    Just [ "Html", "Styled", "Attributes" ] ->
                        ( [ Rule.errorForModule context.moduleKey
                                { message = "Found `css` attribute outside of list"
                                , details =
                                    [ "We first need all instances of `Html.Styled.Attributes.css` to be inside a list of attributes so that we can migrate styles to a sibling `class` attribute."
                                    ]
                                }
                                range
                          ]
                        , { context | ignoredNodes = cssNode :: context.ignoredNodes }
                        )

                    _ ->
                        ( [], context )

            _ ->
                ( []
                , context
                )



-- expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
-- expressionExitVisitor node context =
--     ( [], context )
