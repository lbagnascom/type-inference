module ExprView exposing (viewExpr, highlightParens)

import Expr exposing (Expr(..))
import Html exposing (Html, span, text)
import Html.Attributes exposing (style)


viewExpr : Bool -> Expr -> Html msg
viewExpr showImplicitParens expr =
    expr
        |> fromExprString showImplicitParens
        |> highlightParens


fromExprString : Bool -> Expr -> String
fromExprString showImplicitParens =
    Expr.fromExpr showImplicitParens


highlightParens : String -> Html msg
highlightParens input =
    let
        chars =
            String.toList input

        step char ( depth, currentBuf, rendered ) =
            case char of
                '(' ->
                    ( depth + 1
                    , []
                    , rendered
                        ++ flush currentBuf
                        ++ [ paren "(" depth ]
                    )

                ')' ->
                    ( depth - 1
                    , []
                    , rendered
                        ++ flush currentBuf
                        ++ [ paren ")" (depth - 1) ]
                    )

                _ ->
                    ( depth
                    , char :: currentBuf
                    , rendered
                    )

        ( _, finalBuf, finalRendered ) =
            List.foldl step ( 0, [], [] ) chars
    in
    span []
        (finalRendered ++ flush finalBuf)


flush : List Char -> List (Html msg)
flush buf =
    if List.isEmpty buf then
        []

    else
        [ text (String.fromList (List.reverse buf)) ]


paren : String -> Int -> Html msg
paren p depth =
    span
        [ style "color" (colorFor depth)
        , style "font-weight" "bold"
        ]
        [ text p ]


colorFor : Int -> String
colorFor depth =
    case modBy 6 depth of
        0 ->
            "#d73a49"

        1 ->
            "#6f42c1"

        2 ->
            "#005cc5"

        3 ->
            "#22863a"

        4 ->
            "#e36209"

        _ ->
            "#b31d28"
