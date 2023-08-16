module Optimize exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Maybe.Extra
import RangeDict exposing (RangeDict)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Syntax
import Value exposing (Value)


{-| Reports... REPLACEME

    config =
        [ Optimize.rule
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
elm-review --template miniBill/elm-review-optimize/example --rules Optimize
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "Optimize"
        (Rule.initContextCreator (\() -> initialContext))
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias ModuleContext =
    { current : Frame
    , stack : List Frame
    }


type alias Frame =
    { inferred : Inferred
    , rangesDict : RangeDict Inferred
    }


type alias Inferred =
    Dict String Value


initialContext : ModuleContext
initialContext =
    { current =
        { inferred = Dict.empty
        , rangesDict = RangeDict.empty
        }
    , stack = []
    }


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor _ context =
    case context.stack of
        [] ->
            -- Should never happen
            ( [], initialContext )

        h :: t ->
            ( [], { current = h, stack = t } )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor ((Node range expression) as node) context =
    let
        current : Inferred
        current =
            RangeDict.get range context.current.rangesDict
                |> Maybe.withDefault context.current.inferred

        pushNothing :
            List (Error {})
            -> ( List (Error {}), ModuleContext )
        pushNothing v =
            ( v
            , { context
                | current =
                    { inferred = current
                    , rangesDict = context.current.rangesDict
                    }
                , stack = context.current :: context.stack
              }
            )

        push :
            ( List (Error {}), List ( Range, Inferred ) )
            -> ( List (Error {}), ModuleContext )
        push ( errors, newRanges ) =
            ( errors
            , { context
                | current =
                    { inferred = current
                    , rangesDict =
                        List.foldl
                            (\( k, v ) acc ->
                                RangeDict.insert
                                    k
                                    (case Value.intersectDicts v current of
                                        Just iv ->
                                            iv

                                        Nothing ->
                                            Debug.todo "Intersection during inference should never fail"
                                    )
                                    acc
                            )
                            context.current.rangesDict
                            newRanges
                    }
                , stack = context.current :: context.stack
              }
            )
    in
    case expression of
        RecordAccess record field ->
            case Node.value (Syntax.removeParens record) of
                LetExpression _ ->
                    pushNothing (distributeFieldAccess "a let/in" record field)

                IfBlock _ _ _ ->
                    pushNothing (distributeFieldAccess "an if/then/else" record field)

                CaseExpression _ ->
                    pushNothing (distributeFieldAccess "a case/of" record field)

                _ ->
                    case tryValue node current of
                        Just value ->
                            pushNothing (isConstant value range)

                        Nothing ->
                            pushNothing []

        OperatorApplication opName _ leftChild rightChild ->
            case tryValue node current of
                Just value ->
                    pushNothing (isConstant value range)

                Nothing ->
                    pushNothing (operatorApplicationVisitor range opName leftChild rightChild current)

        Literal _ ->
            pushNothing []

        CharLiteral _ ->
            pushNothing []

        Hex _ ->
            pushNothing []

        Integer _ ->
            pushNothing []

        Floatable _ ->
            pushNothing []

        UnitExpr ->
            pushNothing []

        ListExpr _ ->
            pushNothing []

        IfBlock cond true false ->
            case tryValue node current of
                Just value ->
                    pushNothing (isConstant value range)

                Nothing ->
                    push ( [], visitIfBlock cond true false current )

        _ ->
            case tryValue node current of
                Just value ->
                    pushNothing (isConstant value range)

                Nothing ->
                    pushNothing []


visitIfBlock : Node Expression -> Node Expression -> Node Expression -> Inferred -> List ( Range, Inferred )
visitIfBlock ((Node condRange cond) as condNode) (Node trueRange _) (Node falseRange _) current =
    case Value.getValue condNode current of
        Just _ ->
            -- This will be handled when visiting the child
            []

        Nothing ->
            let
                nodeToInference (Node _ child) =
                    case child of
                        RecordAccess (Node _ (FunctionOrValue [] name)) (Node _ fieldName) ->
                            Just <|
                                \v ->
                                    Dict.singleton name
                                        (Value.Record
                                            { isComplete = False
                                            , fields = Dict.singleton fieldName v
                                            }
                                        )

                        _ ->
                            Nothing

                inferenceAndValue :
                    Node Expression
                    -> Node Expression
                    -> Maybe ( Value -> Dict String Value, Value )
                inferenceAndValue ichild vchild =
                    Maybe.map2 Tuple.pair
                        (nodeToInference ichild)
                        (Value.getValue vchild current)

                branches toInference value =
                    [ ( trueRange
                      , toInference value
                      )
                    , ( falseRange
                      , toInference (Value.invert value)
                      )
                    ]

                isLessThan : { equal : Bool } -> Value -> Maybe Value
                isLessThan { equal } value =
                    Value.getMax value
                        |> Maybe.andThen
                            (\( high, highEq ) ->
                                if isInfinite high then
                                    Nothing

                                else
                                    Just <| Value.number [ ( ( -1 / 0, False ), ( high, highEq && equal ) ) ]
                            )

                isMoreThan : { equal : Bool } -> Value -> Maybe Value
                isMoreThan { equal } value =
                    Value.getMin value
                        |> Maybe.andThen
                            (\( low, lowEq ) ->
                                if isInfinite low then
                                    Nothing

                                else
                                    Just <| Value.number [ ( ( low, lowEq && equal ), ( 1 / 0, False ) ) ]
                            )

                disequation :
                    { equal : Bool }
                    -> ({ equal : Bool } -> Value -> Maybe Value)
                    -> ({ equal : Bool } -> Value -> Maybe Value)
                    -> Node Expression
                    -> Node Expression
                    -> List ( Range, Dict String Value )
                disequation equal straight reversed lchild rchild =
                    case inferenceAndValue lchild rchild of
                        Just ( toInference, rvalue ) ->
                            case straight equal rvalue of
                                Nothing ->
                                    []

                                Just f ->
                                    branches toInference f

                        Nothing ->
                            case inferenceAndValue rchild lchild of
                                Just ( toInference, lvalue ) ->
                                    case reversed equal lvalue of
                                        Nothing ->
                                            []

                                        Just f ->
                                            branches toInference f

                                Nothing ->
                                    []
            in
            case cond of
                OperatorApplication "==" _ lchild rchild ->
                    let
                        inferenceAndValues : Maybe ( Value -> Dict String Value, Value )
                        inferenceAndValues =
                            inferenceAndValue lchild rchild
                                |> Maybe.Extra.orElseLazy
                                    (\_ -> inferenceAndValue lchild rchild)
                    in
                    case inferenceAndValues of
                        Just ( toInference, value ) ->
                            branches toInference value

                        Nothing ->
                            let
                                _ =
                                    Debug.todo <|
                                        "At line "
                                            ++ String.fromInt condRange.start.row
                                            ++ ": "
                                            ++ Syntax.expressionToString lchild
                                            ++ " == "
                                            ++ Syntax.expressionToString rchild
                            in
                            []

                OperatorApplication "<" _ lchild rchild ->
                    disequation { equal = False } isLessThan isMoreThan lchild rchild

                OperatorApplication "<=" _ lchild rchild ->
                    disequation { equal = True } isLessThan isMoreThan lchild rchild

                OperatorApplication ">" _ lchild rchild ->
                    disequation { equal = False } isMoreThan isLessThan lchild rchild

                OperatorApplication ">=" _ lchild rchild ->
                    disequation { equal = True } isMoreThan isLessThan lchild rchild

                _ ->
                    let
                        _ =
                            Debug.todo
                    in
                    []


tryValue : Node Expression -> Inferred -> Maybe String
tryValue node current =
    Value.getValue node current
        |> Maybe.andThen
            (\value ->
                Maybe.map Value.singleToString (Value.toSingle value)
            )


operatorApplicationVisitor :
    Range
    -> String
    -> Node Expression
    -> Node Expression
    -> Inferred
    -> List (Error {})
operatorApplicationVisitor range opName leftChild rightChild context =
    case trySimplify opName leftChild rightChild of
        Just value ->
            isConstant value range

        Nothing ->
            case leftChild of
                Node _ (OperatorApplication lopName lopInfix _ lrchild) ->
                    if opName == lopName && isAssociative opName then
                        case tryValue (Node range (OperatorApplication opName lopInfix lrchild rightChild)) context of
                            Just value ->
                                isConstant value
                                    { start = (Node.range lrchild).start
                                    , end = (Node.range rightChild).end
                                    }

                            _ ->
                                operatorApplicationVisitorAfterParens range opName leftChild rightChild

                    else
                        operatorApplicationVisitorAfterParens range opName leftChild rightChild

                _ ->
                    operatorApplicationVisitorAfterParens range opName leftChild rightChild


operatorApplicationVisitorAfterParens :
    Range
    -> String
    -> Node Expression
    -> Node Expression
    -> List (Error {})
operatorApplicationVisitorAfterParens range opName leftChild rightChild =
    case opName of
        "-" ->
            if expressionEquals leftChild rightChild then
                isConstant "0" range

            else
                []

        _ ->
            []


isAssociative : String -> Bool
isAssociative name =
    name == "+" || name == "*" || name == "++" || name == "&&" || name == "||"


trySimplify : String -> Node Expression -> Node Expression -> Maybe String
trySimplify opName lNode rNode =
    let
        onNumbers : (Int -> Int -> Int) -> (Float -> Float -> Float) -> Maybe String
        onNumbers iop fop =
            case ( Node.value <| Syntax.removeParens lNode, Node.value <| Syntax.removeParens rNode ) of
                ( Integer li, Integer ri ) ->
                    Just <| String.fromInt <| iop li ri

                ( Integer li, Hex ri ) ->
                    Just <| String.fromInt <| iop li ri

                ( Integer li, Floatable rf ) ->
                    Just <| String.fromFloat <| fop (toFloat li) rf

                ( Hex hi, Integer ri ) ->
                    Just <| String.fromInt <| iop hi ri

                ( Hex hi, Hex ri ) ->
                    Just <| String.fromInt <| iop hi ri

                ( Hex hi, Floatable rf ) ->
                    Just <| String.fromFloat <| fop (toFloat hi) rf

                ( Floatable lf, Integer ri ) ->
                    Just <| String.fromFloat <| fop lf (toFloat ri)

                ( Floatable lf, Hex ri ) ->
                    Just <| String.fromFloat <| fop lf (toFloat ri)

                ( Floatable lf, Floatable rf ) ->
                    Just <| String.fromFloat <| fop lf rf

                _ ->
                    Nothing
    in
    case opName of
        "+" ->
            onNumbers (+) (+)

        "-" ->
            onNumbers (-) (-)

        "*" ->
            onNumbers (*) (*)

        _ ->
            Nothing


isConstant : String -> Range -> List (Error {})
isConstant value range =
    [ Rule.errorWithFix
        { message = "Expression is constant"
        , details = [ "The expression can be simplified to the constant " ++ value ]
        }
        range
        [ Fix.replaceRangeBy range value
        ]
    ]


expressionEquals : Node Expression -> Node Expression -> Bool
expressionEquals l r =
    case ( Node.value <| Syntax.removeParens l, Node.value <| Syntax.removeParens r ) of
        ( FunctionOrValue lmod lname, FunctionOrValue rmod rname ) ->
            lmod == rmod && lname == rname

        ( FunctionOrValue _ _, _ ) ->
            False

        ( UnitExpr, UnitExpr ) ->
            True

        ( UnitExpr, _ ) ->
            False

        ( Application lchildren, Application rchildren ) ->
            expressionListEquals lchildren rchildren

        ( Application _, _ ) ->
            False

        ( OperatorApplication lop _ llchild lrchild, OperatorApplication rop _ rlchild rrchild ) ->
            lop == rop && expressionEquals llchild rlchild && expressionEquals lrchild rrchild

        ( OperatorApplication _ _ _ _, _ ) ->
            False

        ( IfBlock lc lt lf, IfBlock rc rt rf ) ->
            expressionListEquals [ lc, lt, lf ] [ rc, rt, rf ]

        ( IfBlock _ _ _, _ ) ->
            False

        ( PrefixOperator lop, PrefixOperator rop ) ->
            lop == rop

        ( PrefixOperator _, _ ) ->
            False

        ( Operator lop, Operator rop ) ->
            lop == rop

        ( Operator _, _ ) ->
            False

        ( Integer li, Integer ri ) ->
            li == ri

        ( Integer li, Hex rh ) ->
            li == rh

        ( Integer li, Floatable rf ) ->
            toFloat li == rf

        ( Integer _, _ ) ->
            False

        ( Hex lh, Integer ri ) ->
            lh == ri

        ( Hex lh, Hex rh ) ->
            lh == rh

        ( Hex lh, Floatable rf ) ->
            toFloat lh == rf

        ( Hex _, _ ) ->
            False

        ( Floatable lf, Integer ri ) ->
            lf == toFloat ri

        ( Floatable lf, Hex rh ) ->
            lf == toFloat rh

        ( Floatable lf, Floatable rf ) ->
            lf == rf

        ( Floatable _, _ ) ->
            False

        ( Negation lc, Negation rc ) ->
            expressionEquals lc rc

        ( Negation _, _ ) ->
            False

        ( Literal ls, Literal rs ) ->
            ls == rs

        ( Literal _, _ ) ->
            False

        ( CharLiteral lc, CharLiteral rc ) ->
            lc == rc

        ( CharLiteral _, _ ) ->
            False

        ( TupledExpression les, TupledExpression res ) ->
            expressionListEquals les res

        ( TupledExpression _, _ ) ->
            False

        ( ListExpr les, ListExpr res ) ->
            expressionListEquals les res

        ( ListExpr _, _ ) ->
            False

        ( RecordAccessFunction lraf, RecordAccessFunction rraf ) ->
            lraf == rraf

        ( RecordAccessFunction _, _ ) ->
            False

        ( GLSLExpression lglsl, GLSLExpression rglsl ) ->
            lglsl == rglsl

        ( GLSLExpression _, _ ) ->
            False

        -- Parens already removed above
        ( ParenthesizedExpression _, _ ) ->
            False

        -- These are complicated, we're not comparing for now
        ( LetExpression _, _ ) ->
            False

        ( CaseExpression _, _ ) ->
            False

        ( LambdaExpression _, _ ) ->
            False

        ( RecordExpr _, _ ) ->
            False

        ( RecordAccess _ _, _ ) ->
            False

        ( RecordUpdateExpression _ _, _ ) ->
            False


expressionListEquals : List (Node Expression) -> List (Node Expression) -> Bool
expressionListEquals lchildren rchildren =
    case lchildren of
        [] ->
            List.isEmpty rchildren

        lhead :: ltail ->
            case rchildren of
                [] ->
                    False

                rhead :: rtail ->
                    if expressionEquals lhead rhead then
                        expressionListEquals ltail rtail

                    else
                        False


distributeFieldAccess : String -> Node Expression -> Node String -> List (Error {})
distributeFieldAccess kind ((Node recordRange _) as record) (Node fieldRange fieldName) =
    let
        { records, withoutParens, withParens } =
            recordLeavesRanges record
    in
    [ let
        removalRange : Range
        removalRange =
            { start = recordRange.end, end = fieldRange.end }
      in
      Rule.errorWithFix
        { message = "Field access can be simplified"
        , details = [ "Accessing the field outside " ++ kind ++ " expression can be simplified to access the field inside it" ]
        }
        removalRange
        (Fix.removeRange removalRange
            :: List.map
                (\leafRange -> Fix.insertAt leafRange.end ("." ++ fieldName))
                (withoutParens ++ records)
            ++ List.concatMap
                (\leafRange ->
                    [ Fix.insertAt leafRange.start "("
                    , Fix.insertAt leafRange.end (")." ++ fieldName)
                    ]
                )
                withParens
        )
    ]


recordLeavesRanges :
    Node Expression
    -> { records : List Range, withoutParens : List Range, withParens : List Range }
recordLeavesRanges (Node range expr) =
    case expr of
        IfBlock _ thenNode elseNode ->
            combineRecordLeavesRanges
                (recordLeavesRanges thenNode)
                (recordLeavesRanges elseNode)

        LetExpression { expression } ->
            recordLeavesRanges expression

        ParenthesizedExpression child ->
            recordLeavesRanges child

        CaseExpression { cases } ->
            List.foldl
                (\( _, e ) -> combineRecordLeavesRanges (recordLeavesRanges e))
                { records = [], withParens = [], withoutParens = [] }
                cases

        RecordExpr _ ->
            { records = [ range ], withParens = [], withoutParens = [] }

        RecordAccess _ _ ->
            { records = [], withParens = [], withoutParens = [ range ] }

        RecordUpdateExpression _ _ ->
            { records = [ range ], withParens = [], withoutParens = [] }

        FunctionOrValue _ _ ->
            { records = [], withParens = [], withoutParens = [ range ] }

        _ ->
            { records = [], withParens = [ range ], withoutParens = [] }


combineRecordLeavesRanges :
    { records : List Range, withoutParens : List Range, withParens : List Range }
    -> { records : List Range, withoutParens : List Range, withParens : List Range }
    -> { records : List Range, withoutParens : List Range, withParens : List Range }
combineRecordLeavesRanges left right =
    { records = left.records ++ right.records
    , withoutParens = left.withoutParens ++ right.withoutParens
    , withParens = left.withParens ++ right.withParens
    }
