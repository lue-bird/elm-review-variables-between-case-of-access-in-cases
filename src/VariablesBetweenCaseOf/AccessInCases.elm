module VariablesBetweenCaseOf.AccessInCases exposing (forbid)

{-|

@docs forbid

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import RangeDict exposing (RangeDict)
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
        |> Rule.withExpressionEnterVisitor
            (\expressionNode context ->
                context |> expressionEnterVisitor expressionNode
            )
        |> Rule.withExpressionExitVisitor
            (\expressionNode context ->
                ( [], context |> expressionExitVisitor expressionNode )
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    { variablesBetweenCaseOf : RangeDict (Set String) }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            { variablesBetweenCaseOf = RangeDict.empty }
        )


expressionExitVisitor : Node Expression -> Context -> Context
expressionExitVisitor expressionNode =
    case expressionNode of
        Node caseOfRange (Expression.CaseExpression _) ->
            \context ->
                { context
                    | variablesBetweenCaseOf =
                        context.variablesBetweenCaseOf
                            |> RangeDict.remove caseOfRange
                }

        Node _ _ ->
            identity


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor expressionNode =
    case expressionNode of
        Node caseOfRange (Expression.CaseExpression caseOf) ->
            \context ->
                ( []
                , { context
                    | variablesBetweenCaseOf =
                        context.variablesBetweenCaseOf
                            |> RangeDict.insert caseOfRange
                                (caseOf.expression |> Node.value |> matchableVariablesInExpression)
                  }
                )

        Node variableRange (Expression.FunctionOrValue [] variableName) ->
            \context ->
                ( let
                    accessedBetweenCaseOf : Bool
                    accessedBetweenCaseOf =
                        RangeDict.any (Set.member variableName)
                            context.variablesBetweenCaseOf
                  in
                  if accessedBetweenCaseOf then
                    [ Rule.error
                        { message =
                            "This variable in the case is used between `case .. of`"
                        , details =
                            [ "Use the information you matched in the case pattern instead of referring to the unmatched variable between `case .. of`."
                            , """This can can prevent forgetting to use certain information and referring to the wrong variables.
For more details, see https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest#why"""
                            ]
                        }
                        variableRange
                    ]

                  else
                    []
                , context
                )

        Node _ _ ->
            \context -> ( [], context )


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
                parts |> setUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

            Expression.ListExpr elements ->
                elements |> setUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

            Expression.Application (applied :: arguments) ->
                case applied |> Node.value of
                    Expression.FunctionOrValue _ appliedName ->
                        if isVariantName appliedName then
                            arguments |> setUnionMap (\(Node _ el) -> el |> matchableVariablesInExpression)

                        else
                            Set.empty

                    _ ->
                        Set.empty


setUnionMap : (a -> Set comparable) -> List a -> Set comparable
setUnionMap change =
    List.foldl
        (\el soFar ->
            Set.union (change el) soFar
        )
        Set.empty


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
