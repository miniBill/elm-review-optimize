module Value exposing (CharValue(..), SingleValue(..), StringValue(..), Value(..), getMax, getMin, getValue, intersect, intersectDicts, invert, singleToString, toSingle)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Interval exposing (Interval)
import Interval.Extra as Interval exposing (Bound)
import List.Extra
import MyDebug
import Set exposing (Set)
import Syntax
import Union exposing (Union)
import Union.Extra as Union


type Value
    = Number Union
    | String StringValue
    | Char CharValue
    | Record { isComplete : Bool, fields : Dict String Value }
    | Bool Bool
    | Unit


type SingleValue
    = SNumber Float
    | SString String
    | SChar Char
    | SRecord (Dict String SingleValue)
    | SBool Bool
    | SUnit


type StringValue
    = OneOfStrings (Set String)
    | NoneOfStrings (Set String)


type CharValue
    = OneOfChars (Set Char)
    | NoneOfChars (Set Char)


toSingle : Value -> Maybe SingleValue
toSingle value =
    case value of
        Number ranges ->
            case Union.toIntervals ranges of
                [ interval ] ->
                    case Interval.unpack interval of
                        ( low, high ) ->
                            if low.value == high.value && not low.open && not high.open then
                                Just <| SNumber low.value

                            else
                                Nothing

                _ ->
                    Nothing

        String (OneOfStrings s) ->
            case Set.toList s of
                [ single ] ->
                    Just <| SString single

                _ ->
                    Nothing

        String _ ->
            Nothing

        Char (OneOfChars c) ->
            case Set.toList c of
                [ single ] ->
                    Just <| SChar single

                _ ->
                    Nothing

        Char _ ->
            Nothing

        Record { isComplete, fields } ->
            if isComplete then
                Dict.foldl
                    (\k v acc ->
                        Maybe.map2
                            (Dict.insert k)
                            (toSingle v)
                            acc
                    )
                    (Just Dict.empty)
                    fields
                    |> Maybe.map SRecord

            else
                Nothing

        Bool b ->
            Just <| SBool b

        Unit ->
            Just SUnit


singleToString : SingleValue -> String
singleToString value =
    Syntax.expressionToString (singleToExpression value)


singleToExpression : SingleValue -> Node Expression.Expression
singleToExpression value =
    Syntax.fakeNode <|
        case value of
            SNumber n ->
                Expression.Floatable n

            SString s ->
                Expression.Literal s

            SChar c ->
                Expression.CharLiteral c

            SRecord fields ->
                fields
                    |> Dict.toList
                    |> List.map toRecordSetter
                    |> Expression.RecordExpr

            SUnit ->
                Expression.UnitExpr

            SBool True ->
                Expression.FunctionOrValue [] "True"

            SBool False ->
                Expression.FunctionOrValue [] "False"


toRecordSetter : ( String, SingleValue ) -> Node Expression.RecordSetter
toRecordSetter ( k, v ) =
    Syntax.fakeNode
        ( Syntax.fakeNode k
        , singleToExpression v
        )


getValue : Int -> Node Expression.Expression -> Dict String Value -> Maybe Value
getValue indent ((Node range expression) as node) context =
    MyDebug.logWrap indent ("getValue " ++ Syntax.expressionToString node) <|
        \indent_ ->
            case expression of
                Expression.UnitExpr ->
                    Just Unit

                Expression.Literal s ->
                    Just (String (OneOfStrings <| Set.singleton s))

                Expression.CharLiteral c ->
                    Just (Char (OneOfChars <| Set.singleton c))

                Expression.Integer i ->
                    Just <| Number <| Union.fromInterval <| Interval.degenerate <| toFloat i

                Expression.Hex h ->
                    Just <| Number <| Union.fromInterval <| Interval.degenerate <| toFloat h

                Expression.Floatable f ->
                    Just <| Number <| Union.fromInterval <| Interval.degenerate f

                Expression.FunctionOrValue [] name ->
                    Dict.get name context

                Expression.FunctionOrValue _ _ ->
                    Nothing

                Expression.RecordAccess child (Node _ field) ->
                    getValue indent_ child context
                        |> Maybe.andThen
                            (\childValue ->
                                case childValue of
                                    Record { fields } ->
                                        Dict.get field fields

                                    _ ->
                                        Nothing
                            )

                Expression.Negation child ->
                    getValue indent_ child context
                        |> Maybe.andThen (numericOp Interval.negate)

                Expression.ParenthesizedExpression child ->
                    getValue indent_ child context

                Expression.IfBlock c t f ->
                    case getValue indent_ c context of
                        Just (Bool True) ->
                            getValue indent_ t context

                        Just (Bool False) ->
                            getValue indent_ f context

                        _ ->
                            case ( getValue indent_ t context, getValue indent_ f context ) of
                                ( Just tv, Just fv ) ->
                                    union tv fv

                                _ ->
                                    Nothing

                Expression.OperatorApplication op _ lchild rchild ->
                    case ( getValue indent_ lchild context, getValue indent_ rchild context ) of
                        ( Just lvalue, Just rvalue ) ->
                            case op of
                                "==" ->
                                    getValueForEquals lvalue rvalue

                                "/=" ->
                                    Maybe.map invert <| getValueForEquals lvalue rvalue

                                "&&" ->
                                    booleanOp2 (&&) lvalue rvalue

                                "<" ->
                                    if isLessThan lvalue rvalue then
                                        Just <| Bool True

                                    else if isMoreThanOrEqualTo lvalue rvalue then
                                        Just <| Bool False

                                    else
                                        Nothing

                                "<=" ->
                                    if isLessThanOrEqualTo lvalue rvalue then
                                        Just <| Bool True

                                    else if isMoreThan lvalue rvalue then
                                        Just <| Bool False

                                    else
                                        Nothing

                                ">" ->
                                    if isMoreThan lvalue rvalue then
                                        Just <| Bool True

                                    else if isLessThanOrEqualTo lvalue rvalue then
                                        Just <| Bool False

                                    else
                                        Nothing

                                ">=" ->
                                    if isMoreThanOrEqualTo lvalue rvalue then
                                        Just <| Bool True

                                    else if isLessThan lvalue rvalue then
                                        Just <| Bool False

                                    else
                                        Nothing

                                "+" ->
                                    numericOp2 Interval.plus lvalue rvalue

                                "-" ->
                                    numericOp2 Interval.minus lvalue rvalue

                                _ ->
                                    (if Dict.isEmpty context then
                                        identity

                                     else
                                        MyDebug.warn
                                            ("Operator application ("
                                                ++ op
                                                ++ ")\n  lchild = "
                                                ++ Syntax.expressionToString lchild
                                                ++ "\n  lvalue = "
                                                ++ toString lvalue
                                                ++ "\n  rchild = "
                                                ++ Syntax.expressionToString rchild
                                                ++ "\n  rvalue = "
                                                ++ toString rvalue
                                                ++ "\n  context = "
                                                ++ contextToString context
                                                ++ "\n range = "
                                                ++ rangeToString range
                                            )
                                    )
                                        Nothing

                        _ ->
                            Nothing

                Expression.ListExpr _ ->
                    MyDebug.warn "getValue > ListExpr" Nothing

                Expression.LetExpression _ ->
                    MyDebug.warn "getValue > LetExpression" Nothing

                Expression.CaseExpression _ ->
                    MyDebug.warn "getValue > CaseExpression" Nothing

                Expression.RecordExpr _ ->
                    MyDebug.warn "getValue > RecordExpr" Nothing

                Expression.RecordUpdateExpression _ _ ->
                    MyDebug.warn "getValue > RecordUpdateExpression" Nothing

                Expression.LambdaExpression _ ->
                    Nothing

                Expression.RecordAccessFunction _ ->
                    Nothing

                Expression.GLSLExpression _ ->
                    Nothing

                Expression.PrefixOperator _ ->
                    Nothing

                Expression.Application _ ->
                    Nothing

                Expression.Operator _ ->
                    Nothing

                Expression.TupledExpression _ ->
                    Nothing


rangeToString : Range -> String
rangeToString { start, end } =
    "( " ++ locationToString start ++ " - " ++ locationToString end ++ " )"


locationToString : Location -> String
locationToString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column


contextToString : Dict String Value -> String
contextToString context =
    context
        |> Dict.toList
        |> List.map (\( k, v ) -> k ++ " = " ++ toString v ++ " ")
        |> String.join ","
        |> (\s -> "( " ++ s ++ ")")


toString : Value -> String
toString value =
    case value of
        Unit ->
            "()"

        _ ->
            MyDebug.todo "toString" "TODO"


numericOp : (Interval -> Interval) -> Value -> Maybe Value
numericOp f lvalue =
    case lvalue of
        Number ranges ->
            Just <| Number <| Union.fromIntervals <| List.map f <| Union.toIntervals ranges

        _ ->
            Nothing


numericOp2 :
    (Interval -> Interval -> Interval)
    -> Value
    -> Value
    -> Maybe Value
numericOp2 f lvalue rvalue =
    case ( lvalue, rvalue ) of
        ( Number lranges, Number rranges ) ->
            Just <| Number <| Union.fromIntervals <| List.Extra.lift2 f (Union.toIntervals lranges) (Union.toIntervals rranges)

        _ ->
            Nothing


isMoreThanOrEqualTo : Value -> Value -> Bool
isMoreThanOrEqualTo lvalue rvalue =
    Maybe.map2
        (\l r -> l.value >= r.value)
        (getMin lvalue)
        (getMax rvalue)
        |> Maybe.withDefault False


isMoreThan : Value -> Value -> Bool
isMoreThan lvalue rvalue =
    Maybe.map2
        (\l r ->
            (l.value > r.value)
                || (l.value == r.value && (l.open || r.open))
        )
        (getMin lvalue)
        (getMax rvalue)
        |> Maybe.withDefault False


isLessThanOrEqualTo : Value -> Value -> Bool
isLessThanOrEqualTo lvalue rvalue =
    Maybe.map2
        (\l r -> l.value <= r.value)
        (getMax lvalue)
        (getMin rvalue)
        |> Maybe.withDefault False


isLessThan : Value -> Value -> Bool
isLessThan lvalue rvalue =
    Maybe.map2
        (\l r ->
            (l.value < r.value)
                || (l.value == r.value && (l.open || r.open))
        )
        (getMax lvalue)
        (getMin rvalue)
        |> Maybe.withDefault False


getMax : Value -> Maybe Bound
getMax value =
    case value of
        Number ranges ->
            Union.upperBound ranges

        _ ->
            Nothing


getMin : Value -> Maybe Bound
getMin value =
    case value of
        Number ranges ->
            Union.lowerBound ranges

        _ ->
            Nothing


booleanOp2 :
    (Bool -> Bool -> Bool)
    -> Value
    -> Value
    -> Maybe Value
booleanOp2 f l r =
    case ( l, r ) of
        ( Bool lb, Bool rb ) ->
            Just <| Bool (f lb rb)

        _ ->
            Nothing


getValueForEquals : Value -> Value -> Maybe Value
getValueForEquals lvalue rvalue =
    let
        optionsToString options =
            options
                |> Set.toList
                |> String.join ", "
    in
    case ( lvalue, rvalue ) of
        ( String (NoneOfStrings loptions), String (OneOfStrings roptions) ) ->
            case Set.toList roptions of
                [ roption ] ->
                    if Set.member roption loptions then
                        Just <| Bool False

                    else if roption == "Aged Brie" || String.startsWith "Backstage" roption then
                        Nothing

                    else
                        MyDebug.warn
                            ("Operator application (==)\n  loptions = None of "
                                ++ optionsToString loptions
                                ++ "\n  roptions = One of "
                                ++ optionsToString roptions
                            )
                            Nothing

                _ ->
                    MyDebug.warn
                        ("Operator application (==)\n  loptions = None of "
                            ++ optionsToString loptions
                            ++ "\n  roptions = One of "
                            ++ optionsToString roptions
                        )
                        Nothing

        ( String (OneOfStrings loptions), String (NoneOfStrings roptions) ) ->
            case Set.toList loptions of
                [ loption ] ->
                    if Set.member loption roptions then
                        Just <| Bool False

                    else
                        MyDebug.warn
                            ("Operator application (==)\n  loptions = One of "
                                ++ optionsToString loptions
                                ++ "\n  roptions = None of "
                                ++ optionsToString roptions
                            )
                            Nothing

                _ ->
                    MyDebug.warn
                        ("Operator application (==)\n  loptions = One of "
                            ++ optionsToString loptions
                            ++ "\n  roptions = None of "
                            ++ optionsToString roptions
                        )
                        Nothing

        ( String (OneOfStrings loptions), String (OneOfStrings roptions) ) ->
            case Set.toList loptions of
                [ loption ] ->
                    case Set.toList roptions of
                        [ roption ] ->
                            Just <| Bool (loption == roption)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            MyDebug.warn
                ("Operator application (==)\n  lvalue = "
                    ++ toString lvalue
                    ++ "\n  rvalue = "
                    ++ toString rvalue
                )
                Nothing


union : Value -> Value -> Maybe Value
union lval rval =
    case ( lval, rval ) of
        ( Unit, Unit ) ->
            Just Unit

        ( Unit, _ ) ->
            Nothing

        ( _, Unit ) ->
            Nothing

        ( Number lranges, Number rranges ) ->
            Just <| Number <| Union.union lranges rranges

        ( Number _, _ ) ->
            Nothing

        ( _, Number _ ) ->
            Nothing

        ( Bool lb, Bool rb ) ->
            if lb == rb then
                Just <| Bool lb

            else
                Nothing

        ( Bool _, _ ) ->
            Nothing

        ( _, Bool _ ) ->
            Nothing

        ( String _, String _ ) ->
            MyDebug.todo "branch '( String _, String _ )' not implemented" Nothing

        ( Char _, String _ ) ->
            MyDebug.todo "branch '( Char _, String _ )' not implemented" Nothing

        ( Record _, String _ ) ->
            MyDebug.todo "branch '( Record _, String _ )' not implemented" Nothing

        ( String _, Char _ ) ->
            MyDebug.todo "branch '( String _, Char _ )' not implemented" Nothing

        ( Char _, Char _ ) ->
            MyDebug.todo "branch '( Char _, Char _ )' not implemented" Nothing

        ( Record _, Char _ ) ->
            MyDebug.todo "branch '( Record _, Char _ )' not implemented" Nothing

        ( String _, Record _ ) ->
            MyDebug.todo "branch '( String _, Record _ )' not implemented" Nothing

        ( Char _, Record _ ) ->
            MyDebug.todo "branch '( Char _, Record _ )' not implemented" Nothing

        ( Record _, Record _ ) ->
            MyDebug.todo "branch '( Record _, Record _ )' not implemented" Nothing


intersect : Value -> Value -> Maybe Value
intersect lval rval =
    case ( lval, rval ) of
        ( Unit, Unit ) ->
            Just Unit

        ( Unit, _ ) ->
            Nothing

        ( _, Unit ) ->
            Nothing

        ( Number lranges, Number rranges ) ->
            Just <| Number <| Union.intersection lranges rranges

        ( Number _, _ ) ->
            Nothing

        ( _, Number _ ) ->
            Nothing

        ( Bool lb, Bool rb ) ->
            if lb == rb then
                Just <| Bool lb

            else
                Nothing

        ( Bool _, _ ) ->
            Nothing

        ( _, Bool _ ) ->
            Nothing

        ( Record lrecord, Record rrecord ) ->
            let
                maybeFields : Maybe (Dict String Value)
                maybeFields =
                    intersectDicts lrecord.fields rrecord.fields
            in
            maybeFields
                |> Maybe.map
                    (\fields ->
                        Record
                            -- The || is counterintuitive but correct:
                            -- if we know from either source that the record is complete
                            -- then applying both constraints still makes it complete.
                            { isComplete = lrecord.isComplete || rrecord.isComplete
                            , fields = fields
                            }
                    )

        ( Record _, _ ) ->
            Nothing

        ( _, Record _ ) ->
            Nothing

        ( String (OneOfStrings loptions), String (NoneOfStrings roptions) ) ->
            Set.diff loptions roptions
                |> OneOfStrings
                |> String
                |> Just

        ( String (NoneOfStrings loptions), String (OneOfStrings roptions) ) ->
            Set.diff roptions loptions
                |> OneOfStrings
                |> String
                |> Just

        ( String (NoneOfStrings loptions), String (NoneOfStrings roptions) ) ->
            Set.union loptions roptions
                |> NoneOfStrings
                |> String
                |> Just

        ( String (OneOfStrings loptions), String (OneOfStrings roptions) ) ->
            Set.intersect loptions roptions
                |> OneOfStrings
                |> String
                |> Just

        ( String _, _ ) ->
            Nothing

        ( _, String _ ) ->
            Nothing

        ( Char (OneOfChars loptions), Char (NoneOfChars roptions) ) ->
            Set.diff loptions roptions
                |> OneOfChars
                |> Char
                |> Just

        ( Char (NoneOfChars loptions), Char (OneOfChars roptions) ) ->
            Set.diff roptions loptions
                |> OneOfChars
                |> Char
                |> Just

        ( Char (NoneOfChars loptions), Char (NoneOfChars roptions) ) ->
            Set.union loptions roptions
                |> NoneOfChars
                |> Char
                |> Just

        ( Char (OneOfChars loptions), Char (OneOfChars roptions) ) ->
            Set.intersect loptions roptions
                |> OneOfChars
                |> Char
                |> Just


intersectDicts : Dict String Value -> Dict String Value -> Maybe (Dict String Value)
intersectDicts ldict rdict =
    Dict.merge
        (\lk lv -> Maybe.map (Dict.insert lk lv))
        (\bk lv rv ->
            Maybe.map2
                (Dict.insert bk)
                (intersect lv rv)
        )
        (\rk rv -> Maybe.map (Dict.insert rk rv))
        ldict
        rdict
        (Just Dict.empty)


invert : Value -> Value
invert value =
    case value of
        Unit ->
            -- x /= () doesn't make sense
            Unit

        Number ranges ->
            let
                everything : Union
                everything =
                    Interval.interval
                        (Interval.excludes (-1 / 0))
                        (Interval.excludes (1 / 0))
                        |> Union.fromInterval
            in
            Number (Union.subtractUnions everything ranges)

        String (OneOfStrings options) ->
            String (NoneOfStrings options)

        String (NoneOfStrings options) ->
            String (OneOfStrings options)

        Char (OneOfChars options) ->
            Char (NoneOfChars options)

        Char (NoneOfChars options) ->
            Char (OneOfChars options)

        Bool b ->
            Bool (not b)

        Record { isComplete, fields } ->
            Record
                { isComplete = isComplete
                , fields = Dict.map (\_ -> invert) fields
                }
