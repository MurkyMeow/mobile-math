module Main exposing (main)

import Browser
import Json.Decode as Decode
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Events exposing (onMouseUp, stopPropagationOn)
import Html.Attributes exposing (class)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { node : MathNode
  , selection : MathNode
  }

init : Model
init =
  { node = BinaryOp (Value "x") Sum (BinaryOp (Function "cos" (Value "y")) Sum Empty)
  , selection = Empty
  }

type Operator
  = Sum
  | Difference
  | Product
  | Fraction

type MathNode
  = Empty
  | Value String
  | Function String MathNode
  | BinaryOp MathNode Operator MathNode

type Msg
  = Change MathNode
  | Select MathNode

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change to ->
      { model | node = to }

    Select node ->
      { model | selection = node }

view : Model -> Html Msg
view model =
  div []
    [ viewMathNode True model.node model.selection ]

viewMathNode : Bool -> MathNode -> MathNode -> Html Msg
viewMathNode showSuggestions node selection =
  let
    viewChild : (MathNode -> MathNode) -> MathNode -> Html Msg
    viewChild transform child =
      Html.map (\msg ->
        case msg of
          Change to ->
            Change (transform to)

          _ ->
            msg
      )
      (viewMathNode showSuggestions child selection)

    suggestions =
      if showSuggestions && selection == node then
        Html.map Change (viewSuggestions node)
      else
        text ""

    attrs =
      [ class "node"
      , stopPropagationOn "mousedown" (Decode.succeed ( Select node, True ))
      ]
  in
  case node of
    Empty ->
      div attrs [ suggestions ]

    Value val ->
      div (attrs ++ [ class "node--value" ])
        [ text val, suggestions ]

    Function template child ->
      div (attrs ++ [ class "node--function" ])
        [ text (template ++ "(")
        , viewChild (\arg -> Function template arg) child
        , text ")"
        , suggestions
        ]

    BinaryOp left op right ->
      case op of
        Fraction ->
          div (attrs ++ [ class "node--fraction" ])
            [ viewChild (\arg -> BinaryOp arg op right) left
            , viewChild (\arg -> BinaryOp left op arg) right
            , suggestions
            ]

        _ ->
          div attrs
            [ viewChild (\arg -> BinaryOp arg op right) left
            , viewOperator op
            , viewChild (\arg -> BinaryOp left op arg) right
            , suggestions
            ]

viewOperator : Operator -> Html a
viewOperator op =
  div [ class "node--operator" ]
    [ case op of
      Sum ->
        text "+"
      
      Difference ->
        text "-"

      Product ->
        text "•"

      Fraction ->
        text "½"
    ]

viewSuggestions : MathNode -> Html MathNode
viewSuggestions node =
  div [ class "node-suggestions" ]
    ( case node of
      Empty ->
        [ viewSuggestion (Value "x")
        , viewSuggestion (Function "cos" Empty)
        ]

      Value _ ->
        [ viewSuggestion (Value "x")
        , viewSuggestion (Value "y")
        , viewSuggestion (Value "z")
        , viewSuggestion (Value "λ")
        , viewSuggestion (BinaryOp node Sum Empty)
        , viewSuggestion Empty
        ]

      Function _ _ ->
        [ viewSuggestion (Function "cos" Empty)
        , viewSuggestion (Function "sin" Empty)
        , viewSuggestion (BinaryOp node Sum Empty)
        , viewSuggestion Empty
        ]

      BinaryOp left _ right ->
        [ viewSuggestion (BinaryOp left Sum right)
        , viewSuggestion (BinaryOp left Difference right)
        , viewSuggestion (BinaryOp left Product right)
        , viewSuggestion (BinaryOp left Fraction right)
        , viewSuggestion left
        , viewSuggestion right
        ]
    )

viewSuggestion : MathNode -> Html MathNode
viewSuggestion suggestion =
  div [ onMouseUp suggestion ]
    ( case suggestion of
      Empty ->
        [ text "▯" ]

      Value val ->
        [ text val ]

      Function template _ ->
        [ text template ]

      BinaryOp left op _ ->
        [ viewSuggestion left, viewOperator op ]
    )
