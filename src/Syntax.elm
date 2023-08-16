module Syntax exposing (expressionToString, fakeNode, removeParens)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Writer


expressionToString : Node Expression -> String
expressionToString expr =
    Elm.Writer.write <|
        Elm.Writer.writeExpression expr


removeParens : Node Expression -> Node Expression
removeParens ((Node _ expression) as node) =
    case expression of
        ParenthesizedExpression child ->
            removeParens child

        _ ->
            node


fakeNode : v -> Node v
fakeNode value =
    Node fakeRange value


fakeRange : Range
fakeRange =
    { start = fakeLocation
    , end = fakeLocation
    }


fakeLocation : Location
fakeLocation =
    { row = -1
    , column = -1
    }
