module Interval.Extra exposing (Bound, minus, negate, pack, plus, unpack)

import Interval exposing (Interval)



-----------
-- Types --
-----------


type alias Bound =
    { value : Float
    , open : Bool
    }



---------------------
-- Transformations --
---------------------


pack : Bound -> Bound -> Interval
pack low high =
    let
        toIntervalBound bound =
            if bound.open then
                Interval.excludes bound.value

            else
                Interval.includes bound.value
    in
    Interval.interval (toIntervalBound low) (toIntervalBound high)


unpack : Interval -> ( Bound, Bound )
unpack interval =
    case ( Interval.lowerBoundValue interval, Interval.upperBoundValue interval ) of
        ( Just low, Just high ) ->
            ( { value = low
              , open = Interval.isLeftOpen interval
              }
            , { value = high
              , open = Interval.isRightOpen interval
              }
            )

        _ ->
            ( { value = 1, open = True }, { value = 1, open = True } )



----------------
-- Arithmetic --
----------------


plus : Interval -> Interval -> Interval
plus l r =
    let
        ( llow, lhigh ) =
            unpack l

        ( rlow, rhigh ) =
            unpack r
    in
    pack
        { value = llow.value + rlow.value
        , open = llow.open || rlow.open
        }
        { value = lhigh.value + rhigh.value
        , open = lhigh.open || rhigh.open
        }


negate : Interval -> Interval
negate interval =
    let
        ( low, high ) =
            unpack interval
    in
    pack (negateBound high) (negateBound low)


negateBound : Bound -> Bound
negateBound bound =
    { bound | value = -bound.value }


minus : Interval -> Interval -> Interval
minus l r =
    plus l (negate r)
