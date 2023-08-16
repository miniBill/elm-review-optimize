module Value exposing (Bound, CharValue(..), SingleValue(..), StringValue(..), Value(..), getMax, getMin, getValue, intersect, intersectDicts, invert, number, singleToString, toSingle)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node exposing (Node(..))
import List.Extra
import Set exposing (Set)
import Syntax


type Value
    = -- For a number x
      Number
        -- Every `((l, le), (r, re))` means `l < x < r` is possible
        -- (with left/right equality if le/re are True).
        -- _l/r could be Infinity_
        (List ( Bound, Bound ))
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


type alias Bound =
    ( Float, Bool )


toSingle : Value -> Maybe SingleValue
toSingle value =
    case value of
        Number [ ( ( l, True ), ( r, True ) ) ] ->
            if l == r then
                Just <| SNumber l

            else
                Nothing

        Number _ ->
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


getValue : Node Expression.Expression -> Dict String Value -> Maybe Value
getValue (Node range expression) context =
    case expression of
        Expression.UnitExpr ->
            Just Unit

        Expression.Literal s ->
            Just (String (OneOfStrings <| Set.singleton s))

        Expression.CharLiteral c ->
            Just (Char (OneOfChars <| Set.singleton c))

        Expression.Integer i ->
            Just (number [ ( ( toFloat i, True ), ( toFloat i, True ) ) ])

        Expression.Hex h ->
            Just (number [ ( ( toFloat h, True ), ( toFloat h, True ) ) ])

        Expression.Floatable f ->
            Just (number [ ( ( f, True ), ( f, True ) ) ])

        Expression.FunctionOrValue [] name ->
            Dict.get name context

        Expression.FunctionOrValue _ _ ->
            Nothing

        Expression.RecordAccess child (Node _ field) ->
            getValue child context
                |> Maybe.andThen
                    (\childValue ->
                        case childValue of
                            Record { fields } ->
                                Dict.get field fields

                            _ ->
                                Nothing
                    )

        Expression.Negation child ->
            case getValue child context of
                Just (Number ranges) ->
                    Just <| number <| List.map negateRange ranges

                _ ->
                    Nothing

        Expression.ParenthesizedExpression child ->
            getValue child context

        Expression.IfBlock c t f ->
            case getValue c context of
                Just (Bool True) ->
                    getValue t context

                Just (Bool False) ->
                    getValue f context

                _ ->
                    case ( getValue t context, getValue f context ) of
                        ( Just tv, Just fv ) ->
                            union tv fv

                        _ ->
                            Nothing

        Expression.OperatorApplication op _ lchild rchild ->
            case ( getValue lchild context, getValue rchild context ) of
                ( Just lvalue, Just rvalue ) ->
                    case op of
                        "==" ->
                            getValueForEquals lvalue rvalue

                        "/=" ->
                            Maybe.map invert <| getValueForEquals lvalue rvalue

                        "&&" ->
                            getValueForBoolean (&&) lvalue rvalue

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
                            numericOp2
                                (\( ( llow, llowEq ), ( lhigh, lhighEq ) ) ( ( rlow, rlowEq ), ( rhigh, rhighEq ) ) ->
                                    ( ( llow + rlow
                                      , llowEq && rlowEq
                                      )
                                    , ( lhigh + rhigh
                                      , lhighEq && rhighEq
                                      )
                                    )
                                )
                                lvalue
                                rvalue

                        _ ->
                            let
                                _ =
                                    if Dict.isEmpty context then
                                        Nothing

                                    else
                                        Debug.todo <|
                                            "Operator application ("
                                                ++ op
                                                ++ ")\n  lchild = "
                                                ++ Syntax.expressionToString lchild
                                                ++ "\n  lvalue = "
                                                ++ Debug.toString lvalue
                                                ++ "\n  rchild = "
                                                ++ Syntax.expressionToString rchild
                                                ++ "\n  rvalue = "
                                                ++ Debug.toString rvalue
                                                ++ "\n  context = "
                                                ++ Debug.toString (Dict.toList context)
                                                ++ "\n range = "
                                                ++ Debug.toString range
                            in
                            Nothing

                _ ->
                    Nothing

        Expression.ListExpr _ ->
            let
                _ =
                    Debug.todo
            in
            Nothing

        Expression.LetExpression _ ->
            let
                _ =
                    Debug.todo
            in
            Nothing

        Expression.CaseExpression _ ->
            let
                _ =
                    Debug.todo
            in
            Nothing

        Expression.RecordExpr _ ->
            let
                _ =
                    Debug.todo
            in
            Nothing

        Expression.RecordUpdateExpression _ _ ->
            let
                _ =
                    Debug.todo
            in
            Nothing

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


isMoreThanOrEqualTo : Value -> Value -> Bool
isMoreThanOrEqualTo lvalue rvalue =
    Maybe.map2
        (\( l, _ ) ( r, _ ) -> l >= r)
        (getMin lvalue)
        (getMax rvalue)
        |> Maybe.withDefault False


isMoreThan : Value -> Value -> Bool
isMoreThan lvalue rvalue =
    Maybe.map2
        (\( l, leq ) ( r, req ) ->
            (l > r)
                || (l == r && (not leq || not req))
        )
        (getMin lvalue)
        (getMax rvalue)
        |> Maybe.withDefault False


isLessThanOrEqualTo : Value -> Value -> Bool
isLessThanOrEqualTo lvalue rvalue =
    Maybe.map2
        (\( l, _ ) ( r, _ ) -> l <= r)
        (getMax lvalue)
        (getMin rvalue)
        |> Maybe.withDefault False


isLessThan : Value -> Value -> Bool
isLessThan lvalue rvalue =
    Maybe.map2
        (\( l, leq ) ( r, req ) ->
            (l < r)
                || (l == r && (not leq || not req))
        )
        (getMax lvalue)
        (getMin rvalue)
        |> Maybe.withDefault False


numericOp :
    (( Bound, Bound ) -> ( Bound, Bound ))
    -> Value
    -> Maybe Value
numericOp f lvalue =
    case lvalue of
        Number ranges ->
            Just <| number <| List.map f ranges

        _ ->
            Nothing


numericOp2 :
    (( Bound, Bound ) -> ( Bound, Bound ) -> ( Bound, Bound ))
    -> Value
    -> Value
    -> Maybe Value
numericOp2 f lvalue rvalue =
    case ( lvalue, rvalue ) of
        ( Number lranges, Number rranges ) ->
            Just <| number <| List.Extra.lift2 f lranges rranges

        _ ->
            Nothing


getMax : Value -> Maybe ( Float, Bool )
getMax value =
    case value of
        Number ranges ->
            Maybe.map Tuple.second (List.Extra.last ranges)

        _ ->
            Nothing


getMin : Value -> Maybe ( Float, Bool )
getMin value =
    case value of
        Number ranges ->
            Maybe.map Tuple.first (List.head ranges)

        _ ->
            Nothing


getValueForBoolean : (Bool -> Bool -> Bool) -> Value -> Value -> Maybe Value
getValueForBoolean f l r =
    case ( l, r ) of
        ( Bool lb, Bool rb ) ->
            Just <| Bool (f lb rb)

        _ ->
            Debug.todo <|
                "Cannot apply boolean to values "
                    ++ Debug.toString l
                    ++ " and "
                    ++ Debug.toString r


getValueForEquals : Value -> Value -> Maybe Value
getValueForEquals lvalue rvalue =
    case ( lvalue, rvalue ) of
        ( String (NoneOfStrings loptions), String (OneOfStrings roptions) ) ->
            case Set.toList roptions of
                [ roption ] ->
                    if Set.member roption loptions then
                        Just <| Bool False

                    else if roption == "Aged Brie" || String.startsWith "Backstage" roption then
                        Nothing

                    else
                        let
                            _ =
                                Debug.todo <|
                                    "Operator application (==)\n  loptions = None of "
                                        ++ Debug.toString (Set.toList loptions)
                                        ++ "\n  roptions = One of "
                                        ++ Debug.toString [ roption ]
                        in
                        Nothing

                _ ->
                    let
                        _ =
                            Debug.todo <|
                                "Operator application (==)\n  loptions = None of "
                                    ++ Debug.toString (Set.toList loptions)
                                    ++ "\n  roptions = One of "
                                    ++ Debug.toString (Set.toList roptions)
                    in
                    Nothing

        ( String (OneOfStrings loptions), String (NoneOfStrings roptions) ) ->
            case Set.toList loptions of
                [ loption ] ->
                    if Set.member loption roptions then
                        Just <| Bool False

                    else
                        let
                            _ =
                                Debug.todo <|
                                    "Operator application (==)\n  loptions = One of "
                                        ++ Debug.toString [ loption ]
                                        ++ "\n  roptions = None of "
                                        ++ Debug.toString (Set.toList roptions)
                        in
                        Nothing

                _ ->
                    let
                        _ =
                            Debug.todo <|
                                "Operator application (==)\n  loptions = One of "
                                    ++ Debug.toString (Set.toList loptions)
                                    ++ "\n  roptions = None of "
                                    ++ Debug.toString (Set.toList roptions)
                    in
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
            let
                _ =
                    Debug.todo <|
                        "Operator application (==)\n  lvalue = "
                            ++ Debug.toString lvalue
                            ++ "\n  rvalue = "
                            ++ Debug.toString rvalue
            in
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
            let
                _ =
                    --dedup
                    Debug.todo
            in
            Just <| number <| lranges ++ rranges

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

        _ ->
            Debug.todo <| "TODO (" ++ Debug.toString lval ++ ", " ++ Debug.toString rval ++ ")"


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
            intersectRanges lranges rranges
                |> number
                |> Just

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


intersectRanges : List ( Bound, Bound ) -> List ( Bound, Bound ) -> List ( Bound, Bound )
intersectRanges lranges rranges =
    -- This assumes that ranges are sorted
    case lranges of
        [] ->
            []

        ( ( llow, llowEq ), ( lhigh, lhighEq ) ) :: ltail ->
            case rranges of
                [] ->
                    []

                ( ( rlow, rlowEq ), ( rhigh, rhighEq ) ) :: rtail ->
                    Debug.todo "intersectRanges"


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


negateRange : ( Bound, Bound ) -> ( Bound, Bound )
negateRange ( l, h ) =
    let
        negateBound : Bound -> Bound
        negateBound ( x, e ) =
            ( -x, e )
    in
    ( negateBound h, negateBound l )


invert : Value -> Value
invert value =
    case value of
        Unit ->
            -- x /= () doesn't make sense
            Unit

        Number [ ( ( low, lowEq ), ( high, highEq ) ) ] ->
            number
                [ ( ( -1 / 0, False )
                  , ( low, not lowEq )
                  )
                , ( ( high, not highEq )
                  , ( 1 / 0, False )
                  )
                ]

        Number _ ->
            Debug.todo "branch 'Number _' not implemented"

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


number : List ( Bound, Bound ) -> Value
number bounds =
    let
        clean : List ( Bound, Bound )
        clean =
            bounds
                |> List.filter
                    (\( ( low, lowEq ), ( high, highEq ) ) ->
                        low < high || (lowEq && highEq)
                    )
                |> List.sortBy (\( ( low, _ ), _ ) -> low)
    in
    if List.any (\( _, ( high, _ ) ) -> isInfinite high && high < 0) clean then
        Debug.todo "Wrong high bound"

    else
        Number clean
