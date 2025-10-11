module TypedExpr exposing (Context, TypedExpr(..), annotate, fromContext, fromTypedExpr, infer, substituteContext, substituteExpr)

import Dict exposing (Dict)
import Expr exposing (Expr(..), Id, foldrExpr)
import Restrictions exposing (Restrictions)
import Set exposing (Set)
import Substitution exposing (Substitution, substitute)
import Type exposing (Type(..), fromType)
import UnicodeSmallDigit exposing (shrinkDigits)
import Utils exposing (lift, lift2, lift3, maybeParens)


type TypedExpr
    = TEVar Id
    | TEAbs Id Type TypedExpr
    | TEApp TypedExpr TypedExpr
    | TEConstTrue
    | TEConstFalse
    | TEIsZero TypedExpr
    | TEConstZero
    | TESucc TypedExpr
    | TEPred TypedExpr
    | TEIf TypedExpr TypedExpr TypedExpr


type alias Context =
    Dict Id Type


fromContext : Context -> String
fromContext c =
    let
        res =
            Dict.toList c
                |> List.map (\( id, t ) -> id ++ ":" ++ fromType t)
                |> List.intersperse ", "
                |> List.foldr (\x y -> x ++ y) ""
    in
    "{" ++ res ++ "}"


substituteContext : Substitution -> Context -> Context
substituteContext s =
    Dict.map (\_ t -> substitute s t)


substituteExpr : Substitution -> TypedExpr -> TypedExpr
substituteExpr s =
    foldrTypedExpr
        TEVar
        (\id t -> TEAbs id (substitute s t))
        TEApp
        TEConstTrue
        TEConstFalse
        TEIsZero
        TEConstZero
        TESucc
        TEPred
        TEIf


freeExprVars : Expr -> Set Id
freeExprVars =
    foldrExpr
        Set.singleton
        (\id rec -> Set.remove id rec)
        (\rec1 rec2 -> Set.union rec1 rec2)
        Set.empty
        Set.empty
        identity
        Set.empty
        identity
        identity
        (\rec1 rec2 rec3 -> Set.union rec1 (Set.union rec2 rec3))


exprContext : Expr -> ( Int, Context )
exprContext expr =
    freeExprVars expr
        |> Set.foldl (\x ( n, d ) -> ( n + 1, Dict.insert x (TVar n) d )) ( 1, Dict.empty )


annotate : Expr -> ( Context, TypedExpr, Int )
annotate expr =
    let
        ( n, context ) =
            exprContext expr

        ( typedExpr, n1 ) =
            annotateHelper expr n
    in
    ( context, typedExpr, n1 )


annotateHelper : Expr -> Int -> ( TypedExpr, Int )
annotateHelper expr n =
    case expr of
        Var id ->
            ( TEVar id, n )

        Abs id e ->
            let
                n1 =
                    n + 1

                ( rec, n2 ) =
                    annotateHelper e n1
            in
            ( TEAbs id (TVar n) rec, n2 )

        App e1 e2 ->
            let
                ( rec1, n1 ) =
                    annotateHelper e1 n

                ( rec2, n2 ) =
                    annotateHelper e2 n1
            in
            ( TEApp rec1 rec2, n2 )

        ConstTrue ->
            ( TEConstTrue, n )

        ConstFalse ->
            ( TEConstFalse, n )

        IsZero e ->
            let
                ( rec, n1 ) =
                    annotateHelper e n
            in
            ( TEIsZero rec, n1 )

        ConstZero ->
            ( TEConstZero, n )

        Succ e ->
            let
                ( rec, n1 ) =
                    annotateHelper e n
            in
            ( TESucc rec, n1 )

        Pred e ->
            let
                ( rec, n1 ) =
                    annotateHelper e n
            in
            ( TEPred rec, n1 )

        If e1 e2 e3 ->
            let
                ( rec1, n1 ) =
                    annotateHelper e1 n

                ( rec2, n2 ) =
                    annotateHelper e2 n1

                ( rec3, n3 ) =
                    annotateHelper e3 n2
            in
            ( TEIf rec1 rec2 rec3, n3 )


foldrTypedExpr :
    (Id -> a) -- Var
    -> (Id -> Type -> a -> a) -- Abs
    -> (a -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (a -> a) -- IsZero
    -> a -- ConstZero
    -> (a -> a) -- Succ
    -> (a -> a) -- Pred
    -> (a -> a -> a -> a) -- If
    -> TypedExpr
    -> a
foldrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            foldrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        TEVar id ->
            fVar id

        TEAbs id t e ->
            fAbs id t (rec e)

        TEApp e1 e2 ->
            fApp (rec e1) (rec e2)

        TEConstTrue ->
            fTrue

        TEConstFalse ->
            fFalse

        TEIsZero e ->
            fIsZero (rec e)

        TEConstZero ->
            fZero

        TESucc e ->
            fSucc (rec e)

        TEPred e ->
            fPred (rec e)

        TEIf e1 e2 e3 ->
            fIf (rec e1) (rec e2) (rec e3)


recrTypedExpr :
    (Id -> a) -- Var
    -> (Id -> Type -> TypedExpr -> a -> a) -- Abs
    -> (TypedExpr -> a -> TypedExpr -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (TypedExpr -> a -> a) -- IsZero
    -> a -- ConstZero
    -> (TypedExpr -> a -> a) -- Succ
    -> (TypedExpr -> a -> a) -- Pred
    -> (TypedExpr -> a -> TypedExpr -> a -> TypedExpr -> a -> a) -- If
    -> TypedExpr
    -> a
recrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            recrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        TEVar id ->
            fVar id

        TEAbs id t e ->
            fAbs id t e (rec e)

        TEApp e1 e2 ->
            fApp e1 (rec e1) e2 (rec e2)

        TEConstTrue ->
            fTrue

        TEConstFalse ->
            fFalse

        TEIsZero e ->
            fIsZero e (rec e)

        TEConstZero ->
            fZero

        TESucc e ->
            fSucc e (rec e)

        TEPred e ->
            fPred e (rec e)

        TEIf e1 e2 e3 ->
            fIf e1 (rec e1) e2 (rec e2) e3 (rec e3)


fromTypedExpr : Bool -> TypedExpr -> String
fromTypedExpr showImplicitParens =
    recrTypedExpr
        shrinkDigits
        (\id t _ rec -> "(λ" ++ shrinkDigits id ++ ":" ++ fromType t ++ ". " ++ rec ++ ")")
        (\e1 rec1 e2 rec2 ->
            maybeParens rec1 ((isApp e1 && showImplicitParens) || isIf e1) ++ " " ++ maybeParens rec2 (isApp e2)
        )
        "true"
        "false"
        (\_ rec -> "isZero(" ++ rec ++ ")")
        "0"
        (\_ rec -> "succ(" ++ rec ++ ")")
        (\_ rec -> "pred(" ++ rec ++ ")")
        (\_ rec1 _ rec2 _ rec3 ->
            "if "
                ++ rec1
                ++ " then "
                ++ rec2
                ++ " else "
                ++ rec3
        )


isApp : TypedExpr -> Bool
isApp expr =
    case expr of
        TEApp _ _ ->
            True

        _ ->
            False


isIf : TypedExpr -> Bool
isIf expr =
    case expr of
        TEIf _ _ _ ->
            True

        _ ->
            False


infer : Context -> TypedExpr -> Int -> ( Maybe ( Type, Restrictions ), Int )
infer context e n =
    case e of
        TEVar id ->
            ( Maybe.map
                (\t -> ( t, Restrictions.empty ))
                (Dict.get id context)
            , n
            )

        TEAbs id varType e1 ->
            lift
                (infer (Dict.insert id varType context))
                (\( bodyType, r1 ) ->
                    ( TAbs varType bodyType, r1 )
                )
                e1
                n

        TEApp e1 e2 ->
            lift2
                (infer context)
                (\( type1, rest1 ) ( type2, rest2 ) ->
                    ( TVar n
                    , Restrictions.insert
                        ( type1, TAbs type2 (TVar n) )
                        (Restrictions.union rest1 rest2)
                    )
                )
                e1
                e2
                (n + 1)

        TEConstTrue ->
            ( Just ( TBool, Restrictions.empty ), n )

        TEConstFalse ->
            ( Just ( TBool, Restrictions.empty ), n )

        TEIsZero e1 ->
            lift (infer context)
                (\( type1, rest1 ) ->
                    ( TBool, Restrictions.insert ( type1, TNat ) rest1 )
                )
                e1
                n

        TEConstZero ->
            ( Just ( TNat, Restrictions.empty ), n )

        TESucc e1 ->
            lift (infer context)
                (\( type1, rest1 ) ->
                    ( TNat, Restrictions.insert ( type1, TNat ) rest1 )
                )
                e1
                n

        TEPred e1 ->
            lift
                (infer context)
                (\( type1, rest1 ) ->
                    ( TNat, Restrictions.insert ( type1, TNat ) rest1 )
                )
                e1
                n

        TEIf e1 e2 e3 ->
            lift3
                (infer context)
                (\( type1, rest1 ) ( type2, rest2 ) ( type3, rest3 ) ->
                    ( type2
                    , Restrictions.union rest1 rest2
                        |> Restrictions.union rest3
                        |> Restrictions.insert ( type1, TBool )
                        |> Restrictions.insert ( type2, type3 )
                    )
                )
                e1
                e2
                e3
                n
