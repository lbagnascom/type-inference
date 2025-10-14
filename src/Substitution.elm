module Substitution exposing
    ( Substitution
    , apply
    , empty
    , fromSubstitution
    , insert
    , substitute
    )

import Type
    exposing
        ( Type(..)
        , foldType
        , fromType
        )
import Utils


type Substitution
    = Substitution (Int -> Type)


empty : Substitution
empty =
    Substitution TVar


substitute : Substitution -> Type -> Type
substitute (Substitution s) =
    foldType s TNat TBool TAbs


apply : Substitution -> Int -> Type
apply (Substitution s) n =
    s n


insert : Int -> Type -> Substitution -> Substitution
insert n t (Substitution s) =
    Substitution <|
        \m ->
            if m == n then
                t

            else
                s m


fromSubstitution : Substitution -> Int -> String
fromSubstitution (Substitution s) n =
    let
        res =
            List.range 1 (n - 1)
                |> List.filter (\k -> TVar k /= s k)
                |> List.map (\k -> fromType (TVar k) ++ "≔" ++ fromType (s k))
                |> Utils.joinWithCommas
    in
    "{" ++ res ++ "}"
