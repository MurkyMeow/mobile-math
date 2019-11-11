module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { node : MathNode
  }

init : Model
init =
  { node = BinaryOp (Value "x") Sum (Value "y")
  }

type Operator
  = Sum
  | Difference
  | Product

type MathNode
  = Empty
  | Value String
  | Function String MathNode
  | BinaryOp MathNode Operator MathNode

type Msg
  = Change MathNode

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change to ->
      { model | node = to }

view : Model -> Html Msg
view model =
  div []
    [ Html.map (\arg -> Change arg) (viewMathNode model.node) ]

viewMathNode : MathNode -> Html MathNode
viewMathNode node =
  case node of
    Empty ->
      div [ class "node" ]
        [ div [ class "node-suggestions" ]
          [ viewSuggestion (Value "xyz")
          , viewSuggestion (Function "f" Empty)
          ]
        ]

    Value val ->
      div [ class "node" ]
        [ text val
        , div [ class "node-suggestions" ]
          [ viewSuggestion (Value "x")
          , viewSuggestion (Value "y")
          , viewSuggestion (Value "z")
          , viewSuggestion (Value "λ")
          , viewSuggestion Empty
          ]
        ]

    Function template child ->
      div [ class "node function" ]
        [ text (template ++ "(")
        , Html.map (\arg -> Function template arg) (viewMathNode child)
        , text ")"
        , div [ class "node-suggestions" ]
          [ viewSuggestion (Function "cos" Empty)
          , viewSuggestion (Function "sin" Empty)
          , viewSuggestion Empty
          ]
        ]

    BinaryOp left operator right ->
      div [ class "node" ]
        [ Html.map (\arg -> BinaryOp arg operator right) (viewMathNode left)
        , viewOperator operator
        , Html.map (\arg -> BinaryOp left operator arg) (viewMathNode right)
        ]

viewOperator : Operator -> Html a
viewOperator operator =
  case operator of
    Sum ->
      text "+"
    
    Difference ->
      text "-"

    Product ->
      text "*"

viewSuggestion : MathNode -> Html MathNode
viewSuggestion suggestion =
  div [ onClick suggestion ]
   [ case suggestion of
      Empty ->
        text "<-"

      Value val ->
        text val

      Function template _ ->
        text template

      BinaryOp left operator right ->
        viewOperator operator
   ]
