module VariablesBetweenCaseOf.AccessInCasesTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import VariablesBetweenCaseOf.AccessInCases exposing (forbid)


all : Test
all =
    describe "VariablesBetweenCaseOf.AccessInCases"
        [ test "should not report an error when cased variable is in function" <|
            \() ->
                """module A exposing (..)
a =
    case identity () of
        () ->
            identity ()
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "should report an error when variable between `case .. of` is used in case" <|
            \() ->
                """module A exposing (..)
a =
    case list of
        [] ->
            ( [], Nothing )
        _ :: _ ->
            ( list, list |> List.minimum )
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message =
                                "This variable in the case is used between `case .. of`"
                            , details =
                                [ "Use the information you matched in the case pattern instead of referring to the unmatched variable between `case .. of`."
                                , """This can can prevent forgetting to use certain information and referring to the wrong variables.
For more details, see https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest#why"""
                                ]
                            , under = "list"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 7, column = 15 }
                                , end = { row = 7, column = 19 }
                                }
                        ]
        ]
