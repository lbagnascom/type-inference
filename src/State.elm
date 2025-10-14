module State exposing (State, lift, lift2, lift3)


type alias State a b =
    b -> ( a, b )


lift : (a -> b) -> State a s -> State b s
lift f g1 n =
    let
        ( res1, n1 ) =
            g1 n
    in
    ( f res1, n1 )


lift2 : (a -> b -> c) -> State a s -> State b s -> State c s
lift2 f fnA fnB n =
    let
        ( res1, n1 ) =
            fnA n

        ( res2, n2 ) =
            fnB n1
    in
    ( f res1 res2, n2 )


lift3 : (a -> b -> c -> d) -> State a s -> State b s -> State c s -> State d s
lift3 f fnA fnB fnC n =
    let
        ( res1, n1 ) =
            fnA n

        ( res2, n2 ) =
            fnB n1

        ( res3, n3 ) =
            fnC n2
    in
    ( f res1 res2 res3, n3 )
