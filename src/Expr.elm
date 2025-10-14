module Expr exposing (Expr(..), Id, foldrExpr, fromExpr, recrExpr)

import UnicodeSmallDigit exposing (shrinkDigits)
import Utils exposing (maybeParens)


type alias Id =
    String


type Expr
    = Var Id
    | Abs Id Expr
    | App Expr Expr
    | ConstTrue
    | ConstFalse
    | IsZero Expr
    | ConstZero
    | Succ Expr
    | Pred Expr
    | If Expr Expr Expr


foldrExpr :
    (Id -> a) -- Var
    -> (Id -> a -> a) -- Abs
    -> (a -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (a -> a) -- IsZero
    -> a -- ConstZero
    -> (a -> a) -- Succ
    -> (a -> a) -- Pred
    -> (a -> a -> a -> a) -- If
    -> Expr
    -> a
foldrExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            foldrExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        Var id ->
            fVar id

        Abs id e ->
            fAbs id (rec e)

        App e1 e2 ->
            fApp (rec e1) (rec e2)

        ConstTrue ->
            fTrue

        ConstFalse ->
            fFalse

        IsZero e ->
            fIsZero (rec e)

        ConstZero ->
            fZero

        Succ e ->
            fSucc (rec e)

        Pred e ->
            fPred (rec e)

        If e1 e2 e3 ->
            fIf (rec e1) (rec e2) (rec e3)


recrExpr :
    (Id -> a) -- Var
    -> (Id -> Expr -> a -> a) -- Abs
    -> (Expr -> a -> Expr -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (Expr -> a -> a) -- IsZero
    -> a -- ConstZero
    -> (Expr -> a -> a) -- Succ
    -> (Expr -> a -> a) -- Pred
    -> (Expr -> a -> Expr -> a -> Expr -> a -> a) -- If
    -> Expr
    -> a
recrExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            recrExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        Var id ->
            fVar id

        Abs id e ->
            fAbs id e (rec e)

        App e1 e2 ->
            fApp e1 (rec e1) e2 (rec e2)

        ConstTrue ->
            fTrue

        ConstFalse ->
            fFalse

        IsZero e ->
            fIsZero e (rec e)

        ConstZero ->
            fZero

        Succ e ->
            fSucc e (rec e)

        Pred e ->
            fPred e (rec e)

        If e1 e2 e3 ->
            fIf e1 (rec e1) e2 (rec e2) e3 (rec e3)


fromExpr : Bool -> Expr -> String
fromExpr showImplicitParens =
    recrExpr
        shrinkDigits
        (\id _ rec -> "(λ" ++ shrinkDigits id ++ ". " ++ rec ++ ")")
        (\e1 rec1 e2 rec2 ->
            maybeParens rec1 ((isApp e1 && showImplicitParens) || isIf e1)
                ++ " "
                ++ maybeParens rec2 (isApp e2)
        )
        "true"
        "false"
        (\_ rec -> "isZero(" ++ rec ++ ")")
        "0"
        (\_ rec -> "succ(" ++ rec ++ ")")
        (\_ rec -> "pred(" ++ rec ++ ")")
        (\_ rec1 _ rec2 _ rec3 ->
            "if " ++ rec1 ++ " then " ++ rec2 ++ " else " ++ rec3
        )


isApp : Expr -> Bool
isApp expr =
    case expr of
        App _ _ ->
            True

        _ ->
            False


isIf : Expr -> Bool
isIf expr =
    case expr of
        If _ _ _ ->
            True

        _ ->
            False
