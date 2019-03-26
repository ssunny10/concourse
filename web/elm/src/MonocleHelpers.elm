module MonocleHelpers exposing (bind, modifyWithEffect)

import Monocle.Lens
import Monocle.Optional



-- bind, like in Haskell


bind :
    Monocle.Optional.Optional a b
    -> (b -> Monocle.Optional.Optional a c)
    -> Monocle.Optional.Optional a c
bind opt f =
    { getOption =
        \a ->
            opt.getOption a
                |> Maybe.andThen (\b -> (f b).getOption a)
    , set =
        \c a ->
            opt.getOption a
                |> Maybe.map (\b -> (f b).set c a)
                |> Maybe.withDefault a
    }


modifyWithEffect :
    Monocle.Optional.Optional a b
    -> (b -> ( b, List c ))
    -> a
    -> ( a, List c )
modifyWithEffect l f m =
    l.getOption m
        |> Maybe.map f
        |> Maybe.map (Tuple.mapFirst (\a -> l.set a m))
        |> Maybe.withDefault ( m, [] )
