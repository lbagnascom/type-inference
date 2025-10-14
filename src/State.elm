module State exposing
    ( State
    , lift
    , lift2
    , lift3
    , new
    )


type alias State a b =
    b -> ( a, b )


new : a -> State a b
new x =
    \n -> ( x, n )


lift : (a -> b) -> State a s -> State b s
lift f g s0 =
    let
        ( res1, s1 ) =
            g s0
    in
    ( f res1, s1 )


lift2 : (a -> b -> c) -> State a s -> State b s -> State c s
lift2 f g1 g2 s0 =
    let
        ( res1, s1 ) =
            g1 s0

        ( res2, s2 ) =
            g2 s1
    in
    ( f res1 res2, s2 )


lift3 : (a -> b -> c -> d) -> State a s -> State b s -> State c s -> State d s
lift3 f g1 g2 g3 s0 =
    let
        ( res1, s1 ) =
            g1 s0

        ( res2, s2 ) =
            g2 s1

        ( res3, s3 ) =
            g3 s2
    in
    ( f res1 res2 res3, s3 )
