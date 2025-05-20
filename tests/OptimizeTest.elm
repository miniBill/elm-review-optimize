module OptimizeTest exposing (all)

import Optimize exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Optimize"
        [ test "should not report an error when the value is not in the same expression" <|
            \() ->
                """module A exposing (..)

q =
    { a = 2
    }


a =
    q.a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)

a =
    let
        q =
            { a = 2
            }
    in
    q.a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is constant"
                            , details = [ "The expression can be simplified to the constant 2" ]
                            , under =
                                """let
        q =
            { a = 2
            }
    in
    q.a"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a =
    2"""
                        ]
        ]
