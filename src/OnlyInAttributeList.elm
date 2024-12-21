module OnlyInAttributeList exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
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
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    Rule.ModuleRuleSchema a ModuleContext
    -> Rule.ModuleRuleSchema { a | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor


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
            , ignoredRange = Nothing
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
    , ignoredRange : Maybe Range
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


getCssDeclaration : ModuleNameLookupTable -> Node LetDeclaration -> Maybe { name : String, rangeToDelete : Range, rangeToKeep : Range }
getCssDeclaration lookupTable node =
    case node of
        Node rangeToDelete (Expression.LetFunction { declaration }) ->
            case declaration of
                Node _ { arguments, name, expression } ->
                    case ( arguments, name, expression ) of
                        ( [], Node _ name_, Node rangeToKeep (Expression.Application (((Node _ (Expression.FunctionOrValue _ "css")) as cssNode) :: _)) ) ->
                            case ModuleNameLookupTable.moduleNameFor lookupTable cssNode of
                                Just [ "Html", "Styled", "Attributes" ] ->
                                    Just { name = name_, rangeToDelete = rangeToDelete, rangeToKeep = rangeToKeep }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

        _ ->
            Nothing


fixesForExpression : List Fix -> { name : String, replacement : String } -> Node Expression -> List Fix
fixesForExpression acc r expression =
    case expression of
        Node _ (Expression.Application nodes) ->
            List.concatMap (fixesForExpression acc r) nodes

        Node _ (Expression.ListExpr nodes) ->
            List.concatMap (fixesForExpression acc r) nodes

        Node rangeToReplace (Expression.FunctionOrValue [] name) ->
            if name == r.name then
                Fix.replaceRangeBy rangeToReplace r.replacement :: acc

            else
                acc

        _ ->
            acc


isWithinIgnoredRange : ModuleContext -> Range -> Bool
isWithinIgnoredRange context range =
    case context.ignoredRange of
        Just ignored ->
            range.start.row
                >= ignored.start.row
                && range.start.column
                >= ignored.start.column
                && range.end.row
                <= ignored.end.row
                && range.end.column
                <= ignored.end.column

        Nothing ->
            False


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    if List.member node context.ignoredNodes then
        ( [], context )

    else
        let
            shouldIgnore range =
                isWithinIgnoredRange context range
        in
        case node of
            Node range (Expression.LetExpression { declarations, expression }) ->
                let
                    cssDeclarations =
                        declarations
                            |> List.filterMap (getCssDeclaration context.lookupTable)

                    allAreIgnored =
                        List.length cssDeclarations == List.length declarations

                    fixes =
                        cssDeclarations
                            |> List.concatMap
                                (\d ->
                                    let
                                        sourceToKeep =
                                            context.sourceCodeExtractor d.rangeToKeep

                                        fixesE =
                                            fixesForExpression [] { name = d.name, replacement = sourceToKeep } expression
                                    in
                                    case ( fixesE, allAreIgnored ) of
                                        ( [], _ ) ->
                                            []

                                        ( _, True ) ->
                                            fixesE
                                                ++ [ Fix.removeRange { start = range.start, end = (Node.range expression).start }
                                                   ]

                                        _ ->
                                            fixesE ++ [ Fix.removeRange d.rangeToDelete ]
                                )
                in
                case fixes of
                    [] ->
                        ( [], context )

                    _ ->
                        ( [ Rule.errorForModuleWithFix context.moduleKey
                                foundOutsideListError
                                range
                                fixes
                          ]
                        , { context | ignoredRange = Just range }
                        )

            Node _ (Expression.ListExpr nodes) ->
                ( []
                , { context
                    | ignoredNodes =
                        context.ignoredNodes
                            ++ List.concatMap (toIgnoredNodes context.lookupTable) nodes
                  }
                )

            Node range (Expression.FunctionOrValue _ "css") ->
                if shouldIgnore range then
                    ( [], context )

                else
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
                if shouldIgnore range then
                    ( [], context )

                else
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


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor (Node range _) context =
    if context.ignoredRange == Just range then
        ( [], { context | ignoredRange = Nothing } )

    else
        ( [], context )
