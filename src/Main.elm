module Main exposing (Model, Msg(..), main)

import Browser
import Browser.Events as Events
import ExprParser exposing (parse)
import ExprView exposing (viewExpr, highlightParens)
import Html exposing (Html, button, div, h2, h3, input, label, li, ol, span, text, textarea, ul)
import Html.Attributes exposing (for, id, style, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Json.Decode as Decode
import Platform.Sub as Sub
import Rectify exposing (rectify)
import Restrictions
    exposing
        ( fromMguError
        , fromRestrictions
        , mgu
        )
import Substitution exposing (fromSubstitution, substitute)
import Type exposing (fromType)
import TypedExpr
    exposing
        ( annotate
        , fromContext
        , fromTypedExpr
        , infer
        , substituteContext
        , substituteExpr
        )
import WizardStep exposing (WizardStep(..), initialStep, next, previous)
import ExprView exposing (highlightParens)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.focusingTextArea then
        Sub.none

    else
        Events.onKeyDown <| Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            Previous

        "ArrowRight" ->
            Next

        _ ->
            NoOp



-- MODEL


type alias Model =
    { showImplicitParens : Bool
    , focusingTextArea : Bool
    , state : WizardStep
    }


init : Model
init =
    { showImplicitParens = False
    , focusingTextArea = False
    , state = WizardStep.initialStep "(\\x.\\y.x) 2 (if x then false else true)"
    }



-- UPDATE


type Msg
    = NoOp
    | Change String
    | FocusTextArea
    | BlurTextArea
    | ToggleImplicitParens
    | Reset
    | Previous
    | Next


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Change newInput ->
            { model | state = initialStep newInput }

        ToggleImplicitParens ->
            { model | showImplicitParens = not model.showImplicitParens }

        Reset ->
            { model | state = init.state }

        Next ->
            { model | state = next model.state }

        Previous ->
            { model | state = previous model.state }

        FocusTextArea ->
            { model | focusingTextArea = True }

        BlurTextArea ->
            { model | focusingTextArea = False }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "width" "70%"
        , style "margin" "40px auto"
        , style "font-family" "sans-serif"
        , style "font-size" "16px"
        ]
        [ h2 [] [ text "Algoritmo I" ]
        , div []
            [ input
                [ onClick ToggleImplicitParens
                , id "toggleParens"
                , type_ "checkbox"
                ]
                []
            , label
                [ for "toggleParens" ]
                [ text "Mostrar paréntesis implícitos" ]
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            (viewStep model.showImplicitParens model.state)
        ]


viewStep : Bool -> WizardStep -> List (Html Msg)
viewStep showParens step =
    case step of
        Parse input ->
            [ h3 [] [ text "Escribí tu expresión" ]
            , exprTextArea input
            ]
                ++ (if String.isEmpty input then
                        []

                    else
                        let
                            parseResult =
                                parse input
                        in
                        case parseResult of
                            Err _ ->
                                [ span [] [ text "No se puede reconocer a qué término se corresponde." ] ]

                            Ok parsedExpr ->
                                [ text "El término es"
                                , stepDiv [ viewExpr showParens parsedExpr ]
                                , stepFooter [ stepStateButton "Empezar" Next ]
                                ]
                   )

        Rectify { parsedExpr } ->
            [ h3 [] [ text "1. Rectificar el término" ]
            , text "Decimos que un término está rectificado si:"
            , ul [ style "margin" "2px" ]
                [ li [] [ text "No hay dos variables ligadas con el mismo nombre." ]
                , li [] [ text "No hay una variable ligada con el mismo nombre que una variable libre." ]
                ]
            , div [ style "margin-bottom" "12px" ] [ text "Siempre podemos rectificar un término a través de α-renombres." ]
            , text "Término inicial"
            , stepDiv [ viewExpr showParens parsedExpr ]
            , div [ style "margin-top" "12px" ] [ text "Término rectificado" ]
            , stepDiv [ viewExpr showParens (rectify parsedExpr) ]
            , stepFooter
                [ stepStateButton "Atrás" Previous
                , stepStateButton "Siguiente" Next
                ]
            ]

        Annotate { rectExpr } ->
            let
                ( context, annotatedExpr, _ ) =
                    annotate rectExpr
            in
            [ h3 [] [ text "2. Anotar el término" ]
            , text "Producimos un contexto Γ₀ y un término M₀"
            , ul [ style "margin" "2px" ]
                [ li [] [ text "El contexto Γ₀ le da tipo a todas las variables libres de U." ]
                , li [] [ text "El término M₀ está anotado de tal modo que Erase(M₀) = U." ]
                ]
            , div [ style "margin-bottom" "12px" ]
                [ text "Todos los tipos y las anotaciones que se agregan son incógnitas frescas."
                ]
            , text "Resultado"
            , stepDiv
                [ div [] [ highlightParens ("M₀ = " ++ fromTypedExpr showParens annotatedExpr) ]
                , div [] [ highlightParens ("Γ₀ = " ++ fromContext context) ]
                ]
            , stepFooter
                [ stepStateButton "Atrás" Previous
                , stepStateButton "Siguiente" Next
                ]
            ]

        Infer { context, annotatedExpr, annotateLastFreshN } ->
            let
                maybeRes =
                    Tuple.first <| infer annotatedExpr context annotateLastFreshN
            in
            case maybeRes of
                Nothing ->
                    [ h3 [] [ text "3. Calcular el conjunto de restricciones" ]
                    , text "Ocurrió un error inesperado al generar las restricciones."
                    , stepFooter
                        [ stepStateButton "Atrás" Previous
                        , stepStateButton "Volver a empezar" Reset
                        ]
                    ]

                Just ( exprType, restrictions ) ->
                    [ h3 [] [ text "3. Calcular el conjunto de restricciones" ]
                    , text "Entrada"
                    , stepDiv
                        [ div [] [ highlightParens ("M₀ = " ++ fromTypedExpr showParens annotatedExpr) ]
                        , div [] [ highlightParens ("Γ₀ = " ++ fromContext context) ]
                        ]
                    , text "Resultado"
                    , stepDiv
                        [ div [] [ highlightParens ("τ = " ++ fromType exprType) ]
                        , div [] [ highlightParens ("E = " ++ fromRestrictions restrictions) ]
                        ]
                    , stepFooter
                        [ stepStateButton "Atrás" Previous
                        , stepStateButton "Siguiente" Next
                        ]
                    ]

        Unify { annotatedExpr, exprType, restrictions, inferLastFreshN, context } ->
            let
                mguRes =
                    mgu restrictions
            in
            case mguRes of
                Err mguError ->
                    [ h3 [] [ text "4. Unificación" ]
                    , text "Dados Γ y M, resultantes de anotar un término rectificado U, una vez calculado I(Γ | M) = (τ | E):"
                    , ol [ style "margin" "2px" ]
                        [ li [] [ text "Calculamos S = mgu(E)." ]
                        , li [] [ text "Si no existe el unificador, el término U no es tipable." ]
                        , li [] [ text "Si existe el unificador, el término U es tipable y devolvemos: S(Γ) ⊢ S(M) : S(τ)" ]
                        ]
                    , div [] [ text "Resultado" ]
                    , div []
                        [ text "El algoritmo de unificación falla con "
                        , text <| fromMguError mguError ++ "."
                        ]
                    , div [] [ text "Por lo tanto, el término no es tipable." ]
                    , stepFooter
                        [ stepStateButton "Atrás" Previous
                        , stepStateButton "Volver a empezar" Reset
                        ]
                    ]

                Ok substitution ->
                    [ h3 [] [ text "4. Unificación" ]
                    , text "Dados Γ y M, resultantes de anotar un término rectificado U, una vez calculado I(Γ | M) = (τ | E):"
                    , ol [ style "margin" "2px" ]
                        [ li [] [ text "Calculamos S = mgu(E)." ]
                        , li [] [ text "Si no existe el unificador, el término U no es tipable." ]
                        , li [] [ text "Si existe el unificador, el término U es tipable y devolvemos: S(Γ) ⊢ S(M) : S(τ)" ]
                        ]
                    , div [] [ text "Resultado" ]
                    , stepDiv
                        [ highlightParens ("S = MGU(E) = " ++ fromSubstitution substitution inferLastFreshN)
                        ]
                    , div
                        [ style "margin-top" "16px", style "margin-bottom" "8px" ]
                        [ text "Por lo tanto, el término es tipable y su juicio más general es" ]
                    , stepDiv
                        [ highlightParens
                            (fromContext (substituteContext substitution context)
                                ++ " ⊢ "
                                ++ fromTypedExpr showParens (substituteExpr substitution annotatedExpr)
                                ++ " : "
                                ++ fromType (substitute substitution exprType))
                        ]
                    , stepFooter
                        [ stepStateButton "Atrás" Previous
                        , stepStateButton "Volver a empezar" Reset
                        ]
                    ]


stepStateButton : String -> Msg -> Html Msg
stepStateButton s m =
    button
        [ onClick m
        , style "padding" "8px 16px"
        , style "cursor" "pointer"
        ]
        [ text s ]


stepFooter : List (Html Msg) -> Html Msg
stepFooter =
    div
        [ style "margin-top" "28px"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "flex-end"
        , style "gap" "6px"
        ]


exprTextArea : String -> Html Msg
exprTextArea input =
    textarea
        [ value input
        , onInput Change
        , onFocus FocusTextArea
        , onBlur BlurTextArea
        , style "margin-bottom" "12px"
        , style "font-size" "16px"
        ]
        []


stepDiv : List (Html msg) -> Html msg
stepDiv xs =
    div
        [ style "background" "#f9f9f9"
        , style "padding" "4px"
        , style "border-radius" "8px"
        , style "font-family" "monospace"
        ]
        xs
