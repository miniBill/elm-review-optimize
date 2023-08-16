module Union.Extra exposing (lowerBound, upperBound)

import Interval.Extra exposing (Bound)
import List.Extra
import Union exposing (Union)


lowerBound : Union -> Maybe Bound
lowerBound union =
    Union.toIntervals union
        |> List.head
        |> Maybe.map (Interval.Extra.unpack >> Tuple.first)


upperBound : Union -> Maybe Bound
upperBound union =
    Union.toIntervals union
        |> List.Extra.last
        |> Maybe.map (Interval.Extra.unpack >> Tuple.second)
