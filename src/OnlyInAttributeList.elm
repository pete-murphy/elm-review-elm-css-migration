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


{-| Reports instances of `Html.Styled.Attributes.css` that are not inside a list of attributes.

    config =
        [ OnlyInAttributeList.rule
        ]


## Fail

    a =
        Html.div [ style ]
            []

    style =
        Attr.css []


## Success

    a =
        Html.div [ Attr.css [] ]
            []


## When (not) to enable this rule

This rule is useful when migrating off of `elm-css`.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template pete-murphy/elm-review-elm-css-migration/example --rules OnlyInAttributeList
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "OnlyInAttributeList" []
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
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


foundOutsideListError : { message : String, details : List String }
foundOutsideListError =
    { message = "Found `css` attribute outside of list"
    , details =
        [ "We first need all instances of `Html.Styled.Attributes.css` to be inside a list of attributes so that we can migrate styles to a sibling `class` attribute."
        ]
    }


toIgnoredNodes : ModuleNameLookupTable -> Node Expression -> List (Node Expression)
toIgnoredNodes lookupTable node =
    case node of
        Node _ (Expression.Application (((Node _ (Expression.FunctionOrValue _ "css")) as cssNode) :: _)) ->
            case ModuleNameLookupTable.moduleNameFor lookupTable cssNode of
                Just [ "Html", "Styled", "Attributes" ] ->
                    [ node, cssNode ]

                _ ->
                    []

        _ ->
            []


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    if List.member node context.ignoredNodes then
        ( [], context )

    else
        case node of
            Node _ (Expression.ListExpr nodes) ->
                ( []
                , { context
                    | ignoredNodes =
                        context.ignoredNodes
                            ++ List.concatMap (toIgnoredNodes context.lookupTable) nodes
                  }
                )

            Node range (Expression.FunctionOrValue _ "css") ->
                case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                    Just [ "Html", "Styled", "Attributes" ] ->
                        ( [ Rule.errorForModule context.moduleKey
                                foundOutsideListError
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
                                foundOutsideListError
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
