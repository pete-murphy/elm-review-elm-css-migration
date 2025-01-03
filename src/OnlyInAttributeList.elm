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


{-| Returns `Nothing` if the let declaration is not an application of
`Html.Styled.Attributes.css`. Otherwise, returns the name of the function being
defined and the ranges to delete and keep. The `rangeToDelete` is the `let
<name> =` part and the `rangeToKeep` is the declaration body. We'll use the
`name` and `rangeToKeep` to replace instances of `name` in the `LetBlock`'s
expression. If all let declarations are CSS declarations, we can delete the whole
`LetBlock`
-}
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
            -- Only match on unqualified names (because we just want to replace what was declared in the let block)
            if name == r.name then
                Fix.replaceRangeBy rangeToReplace r.replacement :: acc

            else
                acc

        Node _ (Expression.FunctionOrValue _ _) ->
            acc

        Node _ (Expression.CaseExpression { cases }) ->
            List.concatMap
                (\( _, exp ) ->
                    fixesForExpression acc r exp
                )
                cases

        Node _ (Expression.ParenthesizedExpression exp) ->
            fixesForExpression acc r exp

        Node _ (Expression.RecordExpr setters) ->
            List.concatMap
                (\(Node _ ( _, exp )) ->
                    fixesForExpression acc r exp
                )
                setters

        Node _ (Expression.IfBlock _ trueNodes falseNodes) ->
            fixesForExpression acc r trueNodes ++ fixesForExpression acc r falseNodes

        Node _ (Expression.LetExpression letExp) ->
            fixesForExpression acc r letExp.expression
                ++ List.concatMap
                    (\d ->
                        case d of
                            Node _ (Expression.LetFunction f) ->
                                case f.declaration of
                                    Node _ dec ->
                                        fixesForExpression acc r dec.expression

                            Node _ (Expression.LetDestructuring _ exp) ->
                                fixesForExpression acc r exp
                    )
                    letExp.declarations

        Node _ Expression.UnitExpr ->
            acc

        Node _ (Expression.OperatorApplication _ _ x y) ->
            fixesForExpression acc r x ++ fixesForExpression acc r y

        Node _ (Expression.PrefixOperator _) ->
            acc

        Node _ (Expression.Operator _) ->
            acc

        Node _ (Expression.Integer _) ->
            acc

        Node _ (Expression.Hex _) ->
            acc

        Node _ (Expression.Floatable _) ->
            acc

        Node _ (Expression.Negation x) ->
            fixesForExpression acc r x

        Node _ (Expression.Literal _) ->
            acc

        Node _ (Expression.CharLiteral _) ->
            acc

        Node _ (Expression.TupledExpression xs) ->
            List.concatMap (fixesForExpression acc r) xs

        Node _ (Expression.LambdaExpression lambda) ->
            fixesForExpression acc r lambda.expression

        Node _ (Expression.RecordAccess x _) ->
            fixesForExpression acc r x

        Node _ (Expression.RecordAccessFunction _) ->
            acc

        Node _ (Expression.RecordUpdateExpression _ setters) ->
            List.concatMap
                (\(Node _ ( _, exp )) ->
                    fixesForExpression acc r exp
                )
                setters

        Node _ (Expression.GLSLExpression _) ->
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

                    -- all declarations are CSS declarations
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

                                        _ ->
                                            fixesE ++ [ Fix.removeRange d.rangeToDelete ]
                                )
                            |> List.append
                                (if allAreIgnored then
                                    [ Fix.removeRange { start = range.start, end = (Node.range expression).start } ]

                                 else
                                    []
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

            Node _ (Expression.OperatorApplication "::" _ node_ _) ->
                ( []
                , { context
                    | ignoredNodes =
                        context.ignoredNodes
                            ++ toIgnoredNodes context.lookupTable node_
                  }
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
