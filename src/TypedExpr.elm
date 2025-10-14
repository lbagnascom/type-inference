module TypedExpr exposing (Context, TypedExpr(..), annotate, fromContext, fromTypedExpr, infer, substituteContext, substituteExpr)

import Dict exposing (Dict)
import Expr exposing (Expr, Id, foldrExpr)
import Restrictions exposing (Restrictions)
import Set exposing (Set)
import State exposing (State, lift, lift2, lift3)
import Substitution exposing (Substitution, substitute)
import Type exposing (Type(..), fromType)
import UnicodeSmallDigit exposing (shrinkDigits)
import Utils exposing (maybeParens)


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
                |> List.foldr (++) ""
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
        Set.remove
        Set.union
        Set.empty
        Set.empty
        identity
        Set.empty
        identity
        identity
        (\rec1 rec2 rec3 ->
            Set.union rec1 (Set.union rec2 rec3)
        )


exprContext : Expr -> ( Int, Context )
exprContext expr =
    freeExprVars expr
        |> Set.foldl
            (\x ( n, d ) -> ( n + 1, Dict.insert x (TVar n) d ))
            ( 1, Dict.empty )


annotate : Expr -> ( Context, TypedExpr, Int )
annotate expr =
    let
        ( n, context ) =
            exprContext expr

        ( typedExpr, n1 ) =
            annotateHelper expr n
    in
    ( context, typedExpr, n1 )


annotateHelper : Expr -> State TypedExpr Int
annotateHelper =
    let
        baseCase t n =
            ( t, n )
    in
    foldrExpr
        (\id n -> ( TEVar id, n ))
        (\id fRec n -> lift (\rec -> TEAbs id (TVar n) rec) fRec (n + 1))
        (lift2 TEApp)
        (baseCase TEConstTrue)
        (baseCase TEConstFalse)
        (lift TEIsZero)
        (baseCase TEConstZero)
        (lift TESucc)
        (lift TEPred)
        (lift3 TEIf)


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
    let
        recWithKeyword keyword _ rec =
            keyword ++ "(" ++ rec ++ ")"
    in
    recrTypedExpr
        shrinkDigits
        (\id t _ rec -> "(λ" ++ shrinkDigits id ++ ":" ++ fromType t ++ ". " ++ rec ++ ")")
        (\e1 rec1 e2 rec2 ->
            maybeParens rec1 ((isApp e1 && showImplicitParens) || isIf e1)
                ++ " "
                ++ maybeParens rec2 (isApp e2)
        )
        "true"
        "false"
        (recWithKeyword "isZero")
        "0"
        (recWithKeyword "succ")
        (recWithKeyword "pred")
        (\_ rec1 _ rec2 _ rec3 -> "if " ++ rec1 ++ " then " ++ rec2 ++ " else " ++ rec3)


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


infer : TypedExpr -> Context -> State (Maybe ( Type, Restrictions )) Int
infer =
    let
        liftM =
            lift << Maybe.map

        baseCase tt _ n =
            ( Just ( tt, Restrictions.empty ), n )

        oneNatSubTerm tt fRec ctx =
            liftM
                (\( type1, rest1 ) -> ( tt, Restrictions.insert ( type1, TNat ) rest1 ))
                (fRec ctx)

        var id ctx n =
            ( Maybe.map (\t -> ( t, Restrictions.empty )) (Dict.get id ctx), n )

        abs id varType fRec ctx =
            liftM
                (\( bodyType, r1 ) -> ( TAbs varType bodyType, r1 ))
                (fRec (Dict.insert id varType ctx))

        app fRec1 fRec2 ctx n =
            (lift2 << Maybe.map2)
                (\( type1, rest1 ) ( type2, rest2 ) ->
                    ( TVar n
                    , Restrictions.insert
                        ( type1, TAbs type2 (TVar n) )
                        (Restrictions.union rest1 rest2)
                    )
                )
                (fRec1 ctx)
                (fRec2 ctx)
                (n + 1)

        true =
            baseCase TBool

        false =
            baseCase TBool

        isZero =
            oneNatSubTerm TBool

        zero =
            baseCase TNat

        succ =
            oneNatSubTerm TNat

        pred =
            oneNatSubTerm TNat

        ifThenElse =
            \fRec1 fRec2 fRec3 ctx ->
                (lift3 << Maybe.map3)
                    (\( type1, rest1 ) ( type2, rest2 ) ( type3, rest3 ) ->
                        ( type2
                        , Restrictions.union rest1 rest2
                            |> Restrictions.union rest3
                            |> Restrictions.insert ( type1, TBool )
                            |> Restrictions.insert ( type2, type3 )
                        )
                    )
                    (fRec1 ctx)
                    (fRec2 ctx)
                    (fRec3 ctx)
    in
    foldrTypedExpr var abs app true false isZero zero succ pred ifThenElse
