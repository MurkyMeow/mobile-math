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
  { node = BinaryOp (Value "x") Sum (Function "cos" (Value "y"))
  , selection = Empty
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
    [ viewMathNode model.node model.selection ]

viewMathNode : MathNode -> MathNode -> Html Msg
viewMathNode node selection =
  let
    viewChild : (MathNode -> MathNode) -> MathNode -> Html Msg
    viewChild transform child =
      Html.map
        (\msg ->
          case msg of
            Change to ->
              Change (transform to)
            
            _ ->
              msg
        )
        (viewMathNode child selection)

    suggestions =
      if selection == node then
        Html.map Change (viewSuggestions node)
      else
        text ""

    parent = div
      [ class "node"
      , stopPropagationOn "mousedown" (Decode.succeed ( Select node, True ))
      ]
  in
  case node of
    Empty ->
      parent [ suggestions ]

    Value val ->
      parent [ text val, suggestions ]

    Function template child ->
      parent
        [ text (template ++ "(")
        , viewChild (\arg -> Function template arg) child
        , text ")"
        , suggestions
        ]

    BinaryOp left operator right ->
      parent
        [ viewChild (\arg -> BinaryOp arg operator right) left
        , viewOperator operator
        , viewChild (\arg -> BinaryOp left operator arg) right
        , suggestions
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

viewSuggestions : MathNode -> Html MathNode
viewSuggestions node =
  div [ class "node-suggestions" ]
    (case node of
      Empty ->
        [ viewSuggestion (Value "xyz")
        , viewSuggestion (Function "f" Empty)
        ]

      Value _ ->
        [ viewSuggestion (Value "x")
        , viewSuggestion (Value "y")
        , viewSuggestion (Value "z")
        , viewSuggestion (Value "λ")
        , viewSuggestion Empty
        ]

      Function _ _ ->
        [ viewSuggestion (Function "cos" Empty)
        , viewSuggestion (Function "sin" Empty)
        , viewSuggestion Empty
        ]

      BinaryOp _ _ _ ->
        []
    )

viewSuggestion : MathNode -> Html MathNode
viewSuggestion suggestion =
  div [ onMouseUp suggestion ]
    [ case suggestion of
        Empty ->
          text "⬅"

        Value val ->
          text val

        Function template _ ->
          text template

        BinaryOp left operator _ ->
          viewOperator operator
    ]
