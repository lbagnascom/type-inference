module WizardStep exposing
    ( WizardStep(..)
    , initialStep
    , next
    , previous
    )

import Expr exposing (Expr)
import ExprParser exposing (parse)
import Rectify exposing (rectify)
import Restrictions
    exposing
        ( Restrictions
        )
import Type exposing (Type)
import TypedExpr
    exposing
        ( Context
        , TypedExpr
        , annotate
        , infer
        )


type WizardStep
    = Parse String
    | Rectify
        { input : String
        , parsedExpr : Expr
        }
    | Annotate
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        }
    | Infer
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , annotateLastFreshN : Int
        }
    | Unify
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , annotateLastFreshN : Int
        , exprType : Type
        , restrictions : Restrictions
        , inferLastFreshN : Int
        }


initialStep : String -> WizardStep
initialStep s =
    Parse s


next : WizardStep -> WizardStep
next step =
    case step of
        Parse input ->
            case parse input of
                Err _ ->
                    Parse input

                Ok parsedExpr ->
                    Rectify { input = input, parsedExpr = parsedExpr }

        Rectify { input, parsedExpr } ->
            Annotate
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectify parsedExpr
                }

        Annotate { input, parsedExpr, rectExpr } ->
            let
                ( context, annotatedExpr, nextFreshN ) =
                    annotate rectExpr
            in
            Infer
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , annotateLastFreshN = nextFreshN
                }

        Infer { input, parsedExpr, rectExpr, context, annotatedExpr, annotateLastFreshN } ->
            let
                ( maybeRes, inferLastFreshN ) =
                    infer annotatedExpr context annotateLastFreshN
            in
            case maybeRes of
                Nothing ->
                    step

                Just ( exprType, restrictions ) ->
                    Unify
                        { input = input
                        , parsedExpr = parsedExpr
                        , rectExpr = rectExpr
                        , context = context
                        , annotatedExpr = annotatedExpr
                        , annotateLastFreshN = annotateLastFreshN
                        , exprType = exprType
                        , restrictions = restrictions
                        , inferLastFreshN = inferLastFreshN
                        }

        Unify _ ->
            step


previous : WizardStep -> WizardStep
previous step =
    case step of
        Parse _ ->
            step

        Rectify { input } ->
            Parse input

        Annotate { input, parsedExpr } ->
            Rectify { input = input, parsedExpr = parsedExpr }

        Infer { input, parsedExpr, rectExpr } ->
            Annotate
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                }

        Unify { input, parsedExpr, rectExpr, context, annotatedExpr, annotateLastFreshN } ->
            Infer
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , annotateLastFreshN = annotateLastFreshN
                }
