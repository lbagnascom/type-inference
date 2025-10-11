module Restrictions exposing
    ( MguError(..)
    , Restriction
    , Restrictions
    , empty
    , fromMguError
    , fromRestrictions
    , insert
    , mgu
    , singleton
    , union
    )

import List
import Substitution exposing (Substitution, substitute)
import Type exposing (Type(..), fromType, hasVar, replaceVar)


type alias Restriction =
    ( Type, Type )


type alias Restrictions =
    List Restriction


empty : Restrictions
empty =
    []


singleton : Restriction -> Restrictions
singleton r =
    [ r ]


member : Restriction -> Restrictions -> Bool
member =
    List.member


insert : Restriction -> Restrictions -> Restrictions
insert r c =
    if member r c then
        c

    else
        r :: c


union : Restrictions -> Restrictions -> Restrictions
union c1 c2 =
    c1 ++ List.filter (\x -> not <| List.member x c1) c2


type MguError
    = Clash Type Type
    | OccursCheck Type Type


fromMguError : MguError -> String
fromMguError mguErr =
    let
        errorMessage t1 t2 =
            fromType t1 ++ " y " ++ fromType t2
    in
    case mguErr of
        Clash t1 t2 ->
            "Clash/Colisión entre " ++ errorMessage t1 t2

        OccursCheck t1 t2 ->
            "OccursCheck entre " ++ errorMessage t1 t2


mgu : Restrictions -> Result MguError Substitution
mgu ys =
    case ys of
        [] ->
            Ok Substitution.empty

        ( type1, type2 ) :: xs ->
            case ( type1, type2 ) of
                ( TNat, TNat ) ->
                    mgu xs

                ( TBool, TBool ) ->
                    mgu xs

                ( TAbs a1 a2, TAbs b1 b2 ) ->
                    mgu (insert ( a1, b1 ) (insert ( a2, b2 ) xs))

                ( TVar n, TVar m ) ->
                    if n == m then
                        mgu xs

                    else
                        let
                            replaceVarN =
                                replaceVar n type2
                        in
                        Result.map (\s -> Substitution.insert n (substitute s type2) s) <|
                            mgu (List.map (Tuple.mapBoth replaceVarN replaceVarN) xs)

                ( _, TVar _ ) ->
                    mgu <| insert ( type2, type1 ) xs

                ( TVar n, _ ) ->
                    if hasVar n type2 then
                        Err (OccursCheck type1 type2)

                    else
                        let
                            replaceVarN =
                                replaceVar n type2
                        in
                        Result.map (\s -> Substitution.insert n (substitute s type2) s) <|
                            mgu (List.map (Tuple.mapBoth replaceVarN replaceVarN) xs)

                _ ->
                    Err (Clash type1 type2)


fromRestriction : Restriction -> String
fromRestriction ( t1, t2 ) =
    fromType t1 ++ "≟" ++ fromType t2


fromRestrictions : Restrictions -> String
fromRestrictions rs =
    let
        res =
            List.map fromRestriction rs
                |> List.intersperse ", "
                |> List.foldr (\s1 s2 -> s1 ++ s2) ""
    in
    "{" ++ res ++ "}"
