module VariablesBetweenCaseOf.AccessInCasesTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import VariablesBetweenCaseOf.AccessInCases exposing (forbid)


all : Test
all =
    describe "VariablesBetweenCaseOf.AccessInCases"
        [ test "should not report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
