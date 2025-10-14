module Utils exposing (joinWithCommas, maybeParens, until)


until : (a -> Bool) -> (a -> a) -> a -> a
until p f z =
    if p z then
        z

    else
        until p f (f z)


maybeParens : String -> Bool -> String
maybeParens s b =
    if b then
        "(" ++ s ++ ")"

    else
        s


joinWithCommas : List String -> String
joinWithCommas xs =
    List.intersperse ", " xs
        |> List.foldr (++) ""
