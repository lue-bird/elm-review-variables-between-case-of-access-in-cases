module VariablesBetweenCaseOf.AccessInCases exposing (forbid)

{-|

@docs forbid

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports when a variable between `case .. of` is used in any of the cases

    config =
        [ VariablesBetweenCaseOf.AccessInCases.forbid
        ]


## reported

    case arguments of
        [] ->
            "no arguments"

        _ :: _ ->
            arguments |> String.join ", "

The rule collects variables even inside tuples, variants and lists.


## not reported

    case arguments of
        [] ->
            "no arguments"

        firstArg :: args1Up ->
            (firstArg :: args1Up) |> String.join ", "

The rule does not collect variables inside records, functions, operations, ...

    case originalList |> List.drop 4 of
        [] ->
            "less than 5 arguments"

        _ :: _
            originalList |> String.join ", "


## When (not) to use

This rule is not useful when you would fix

    case list of
        [] ->
            "no arguments"

        _ :: _ ->
            list |> String.join ", "

to

    case list of
        [] ->
            "no arguments"

        list_ ->
            list_ |> String.join ", "

That's not the purpose of this rule!

The motivation is preventing you from forgetting to use certain information and referring to the wrong variables.
For more details, check [the readme](https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/1.0.0#why)

-}
forbid : Rule
forbid =
    Rule.newModuleRuleSchemaUsingContextCreator "VariablesBetweenCaseOf.AccessInCases.forbid" initialContext
        |> Rule.withSimpleExpressionVisitor
            (\expressionNode ->
                expressionVisitor expressionNode
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    ()


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator identity


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor expressionNode =
    case recursiveExpressionVisitor { variablesBetweenCaseOf = Set.empty } expressionNode of
        Nothing ->
            []

        Just error ->
            [ error ]


recursiveExpressionVisitor : { variablesBetweenCaseOf : Set String } -> Node Expression -> Maybe (Rule.Error {})
recursiveExpressionVisitor context expressionNode =
    case expressionNode of
        Node _ (Expression.CaseExpression caseOf) ->
            listFirstJustMap
                (\( Node _ _, caseExpression ) ->
                    recursiveExpressionVisitor
                        { variablesBetweenCaseOf =
                            context.variablesBetweenCaseOf
                                |> Set.union
                                    (caseOf.expression |> Node.value |> matchableVariablesInExpression)
                        }
                        caseExpression
                )
                caseOf.cases

        Node variableRange (Expression.FunctionOrValue [] variableName) ->
            if Set.member variableName context.variablesBetweenCaseOf then
                Rule.error
                    { message =
                        "This variable in the case is used between `case .. of`"
                    , details =
                        [ "Use the information you matched in the case pattern instead of referring to the unmatched variable between `case .. of`."
                        , """This can can prevent forgetting to use certain information and referring to the wrong variables.
For more details, see https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest#why"""
                        ]
                    }
                    variableRange
                    |> Just

            else
                Nothing

        Node _ nonFunctionOrValueOrCaseOf ->
            nonFunctionOrValueOrCaseOf
                |> subExpressionsThatCanContainVariablesOrCaseOfs
                |> listFirstJustMap (\sub -> recursiveExpressionVisitor context sub)


matchableVariablesInExpression : Expression -> Set String
matchableVariablesInExpression =
    \expression ->
        case expression of
            Expression.Floatable _ ->
                Set.empty

            Expression.UnitExpr ->
                Set.empty

            Expression.Integer _ ->
                Set.empty

            Expression.Hex _ ->
                Set.empty

            Expression.Negation _ ->
                Set.empty

            Expression.Operator _ ->
                Set.empty

            Expression.PrefixOperator _ ->
                Set.empty

            Expression.OperatorApplication _ _ _ _ ->
                Set.empty

            Expression.RecordAccessFunction _ ->
                Set.empty

            Expression.CaseExpression _ ->
                Set.empty

            Expression.IfBlock _ _ _ ->
                Set.empty

            Expression.LambdaExpression _ ->
                Set.empty

            Expression.GLSLExpression _ ->
                Set.empty

            Expression.LetExpression _ ->
                Set.empty

            Expression.CharLiteral _ ->
                Set.empty

            Expression.Literal _ ->
                Set.empty

            Expression.RecordExpr _ ->
                Set.empty

            Expression.RecordAccess _ _ ->
                Set.empty

            Expression.RecordUpdateExpression _ _ ->
                Set.empty

            Expression.FunctionOrValue (_ :: _) _ ->
                Set.empty

            Expression.FunctionOrValue [] name ->
                if name |> isVariableName then
                    name |> Set.singleton

                else
                    Set.empty

            Expression.Application [] ->
                Set.empty

            Expression.ParenthesizedExpression (Node _ inParens) ->
                inParens |> matchableVariablesInExpression

            Expression.TupledExpression parts ->
                parts |> listSetUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

            Expression.ListExpr elements ->
                elements |> listSetUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

            Expression.Application (applied :: arguments) ->
                case applied |> Node.value of
                    Expression.FunctionOrValue _ appliedName ->
                        if isVariantName appliedName then
                            arguments |> listSetUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

                        else
                            Set.empty

                    _ ->
                        Set.empty


{-| Get all immediate child expressions of an expression but not stuff like functions or `{ this | ... }`
because they can never be a `case .. of` or a variable.
-}
subExpressionsThatCanContainVariablesOrCaseOfs : Expression -> List (Node Expression)
subExpressionsThatCanContainVariablesOrCaseOfs expression =
    case expression of
        Expression.LetExpression letBlock ->
            letBlock.declarations
                |> List.map Node.value
                |> List.map
                    (\letDeclaration ->
                        case letDeclaration of
                            Expression.LetFunction { declaration } ->
                                declaration |> Node.value |> .expression

                            Expression.LetDestructuring _ expression_ ->
                                expression_
                    )
                |> (::) letBlock.expression

        Expression.ListExpr expressions ->
            expressions

        Expression.TupledExpression expressions ->
            expressions

        Expression.RecordExpr fields ->
            fields |> List.map (\(Node _ ( _, value )) -> value)

        Expression.RecordUpdateExpression _ updaters ->
            updaters |> List.map (\(Node _ ( _, newValue )) -> newValue)

        Expression.RecordAccess _ _ ->
            []

        Expression.Application [] ->
            []

        Expression.Application (_ :: arguments) ->
            arguments

        Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, caseExpression ) -> caseExpression))

        Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Expression.UnitExpr ->
            []

        Expression.Integer _ ->
            []

        Expression.Hex _ ->
            []

        Expression.Floatable _ ->
            []

        Expression.Literal _ ->
            []

        Expression.CharLiteral _ ->
            []

        Expression.GLSLExpression _ ->
            []

        Expression.RecordAccessFunction _ ->
            []

        Expression.FunctionOrValue _ _ ->
            []

        Expression.Operator _ ->
            []

        Expression.PrefixOperator _ ->
            []


{-| I know that this will give us false negatives but elm's checks are rather weird so... better than false positives
-}
isVariantName : String -> Bool
isVariantName name =
    case name |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


{-| I know that this will give us false negatives but elm's checks are rather weird so... better than false positives
-}
isVariableName : String -> Bool
isVariableName name =
    case name |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isLower


listSetUnionMap : (a -> Set comparable) -> List a -> Set comparable
listSetUnionMap change =
    List.foldl
        (\el soFar ->
            Set.union (change el) soFar
        )
        Set.empty


listFirstJustMap : (a -> Maybe b) -> (List a -> Maybe b)
listFirstJustMap elementToMaybe =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                case elementToMaybe head of
                    Just found ->
                        Just found

                    Nothing ->
                        listFirstJustMap elementToMaybe tail
