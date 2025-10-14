module Type exposing (Type(..), foldType, fromType, hasVar, replaceVar)

import String exposing (fromInt)
import UnicodeSmallDigit exposing (shrinkDigits)
import Utils exposing (maybeParens)


type Type
    = TVar Int
    | TNat
    | TBool
    | TAbs Type Type


foldType :
    (Int -> a)
    -> a
    -> a
    -> (a -> a -> a)
    -> Type
    -> a
foldType fVar fNat fBool fAbs t =
    case t of
        TVar n ->
            fVar n

        TNat ->
            fNat

        TBool ->
            fBool

        TAbs t1 t2 ->
            let
                rec =
                    foldType fVar fNat fBool fAbs
            in
            fAbs (rec t1) (rec t2)


recrType :
    (Int -> a)
    -> a
    -> a
    -> (Type -> a -> Type -> a -> a)
    -> Type
    -> a
recrType fVar fNat fBool fAbs t =
    case t of
        TVar n ->
            fVar n

        TNat ->
            fNat

        TBool ->
            fBool

        TAbs t1 t2 ->
            let
                rec =
                    recrType fVar fNat fBool fAbs
            in
            fAbs t1 (rec t1) t2 (rec t2)


fromType : Type -> String
fromType =
    recrType
        (\n -> "X" ++ shrinkDigits (fromInt n))
        "Nat"
        "Bool"
        (\t1 rec1 _ rec2 -> maybeParens rec1 (isAbs t1) ++ "→" ++ rec2)


isAbs : Type -> Bool
isAbs t =
    case t of
        TAbs _ _ ->
            True

        _ ->
            False


replaceVar : Int -> Type -> Type -> Type
replaceVar n t =
    foldType
        (\varN ->
            if varN == n then
                t

            else
                TVar varN
        )
        TNat
        TBool
        TAbs


hasVar : Int -> Type -> Bool
hasVar n =
    foldType
        (\m -> n == m)
        False
        False
        (\rec1 rec2 -> rec1 || rec2)
