module Main exposing (..)

import Array exposing (..)
import Html exposing (Html, text, div, h1, button, p, br, ul, li, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Random
import Utils exposing (..)

---- MODEL ----

type alias Model =
    {
      state : String
      , problems : Array Problem
      , random1 : Int
      , random2 : Int
    }

type alias Problem =
    {
      operands : List String
      , operator : String
      , answer : String
      , result: String

    }

init : ( Model, Cmd Msg )
init =
    ( {
        state = "NOT STARTED"
        , problems =  Array.empty
        , random1 = 10000
        , random2 = 20000
         }, Cmd.none )

---- UPDATE ----
type Msg
    = START
    | NewRandom1 Int
    | NewRandom2 Int
    | CheckAnswer Int String String
    | END



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        START ->
          ( { model | state = "STARTED"}, Random.generate NewRandom1 (Random.int 1 9) )

        NewRandom1 num ->
            ({model | random1 = num} , Random.generate NewRandom2 (Random.int 1 9))

        NewRandom2 num ->
            let
              op1 = model.random1
              op2 = num
              eresult = toString( op1 + op2 )
              problem =  { operands = [toString op1 , toString num], operator = "-" , answer = eresult, result = ""}
              new_problems = problem :: (Array.toList model.problems)
            in
              ({model | problems = Array.fromList new_problems  } , Cmd.none)

        END ->
            init

        CheckAnswer id num1 num ->
            let
              ans =  if (num1 == num) then
                       "😀"
                     else
                       "😞"
            in
              (model.problems
                |> Array.get id
                |>Maybe.map
                  (\p -> {p | result = ans})
                |> Maybe.map
                  (\p -> Array.set id p model.problems)
                |> Maybe.map (\arr -> {model | problems = arr })
                |> Maybe.withDefault model
              , Cmd.none
              )


---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container-fluid"]
        [
        header model
        , body model
        , debugView model
        ]

debugView : Model -> Html msg
debugView model =
    div []
    [
      text (toString model)
    ]

header : Model -> Html Msg
header model =
    case model.state of
       "STARTED" ->
           startHtml "Lets get it"

       "NOT STARTED" ->
           notStartedHtml "Welcome to Probs!"

       "END" ->
           notStartedHtml "Welcome to Probs!"
       _ ->
          div [] []

notStartedHtml : String -> Html Msg
notStartedHtml string =
    div [class "container" ][
      div [class "row justify-content-md-center"]
        [ h1 [] [text string]]
      , div [class "row justify-content-md-center"]
         [ button [onClick START, class "btn btn-outline-primary" ] [text "Start"] ]
     ]

startHtml : String -> Html Msg
startHtml string  =
  div [class "container" ][
    div [class "row justify-content-md-center"]
       [ h1 [] [text string]]
   , div [class "row justify-content-md-center"]
      [
        button [onClick START, class "btn btn-outline-primary" ] [text "ADD"]
        ,button [onClick END, class "btn btn-outline-primary" ] [text "END"]
      ]
  ]

body : Model -> Html Msg
body model =
   div []
      [
        text "THis Problems"
        , ul []
             (model.problems
              |> Array.indexedMap
               (\i prob -> li [class "mt-3"]
                  [
                    text (renderProblem prob.operands)
                    , input [ onInput (CheckAnswer i prob.answer) ] []
                    , text prob.result
                  ]
                )
              |> Array.toList )
--          (List.map (\prob -> li [class "mt-3"]
--            [ text (renderProblem prob.operands)
--              , input [ onInput (CheckAnswer prob.id prob.answer) ] []
--            ]) model.problems )
      ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

