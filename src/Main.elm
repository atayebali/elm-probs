module Main exposing (..)

import Array exposing (..)
import Html exposing (Html, text, div, h1, button, p, br, ul, li, input)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Random
import Utils exposing (..)

---- MODEL ----

type alias Model =
    {
      state : String
      , problems : Array Problem
      , operator : String
      , random1 : Int
      , random2 : Int
      , index : Int
    }

type alias Problem =
    {
      operands : List String
      , answer : String
      , userAnswer : String
      , result: String
    }

problemBuilder : Int -> Int-> String -> Problem
problemBuilder op1 num result =
    { operands = [toString op1 , toString num], answer = result, userAnswer = "", result = ""}

init : ( Model, Cmd Msg )
init =
    ( {
        state = "NOT STARTED"
        , problems =  Array.empty
        , random1 = 10000
        , random2 = 20000
        , operator = "+"
        , index = -1
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
            ( { model | state = "STARTED", index = model.index + 1 }, Random.generate NewRandom1 (Random.int 1 9) )

        NewRandom1 num ->
            ({model | random1 = num} , Random.generate NewRandom2 (Random.int 1 9))

        NewRandom2 num ->
            let
              op1 = model.random1
              op2 = num
              operator = model.operator
              eresult = toString( calculate op1 operator op2 )
              problem =  problemBuilder op1 num eresult
              new_problems = problem :: (Array.toList model.problems)
            in
              ({model | problems = Array.fromList new_problems  } , Cmd.none)

        END ->
            init

        CheckAnswer id num1 num ->
            let
              ans =  if (num1 == num) then
                       happy num
                     else
                       sad num
            in
              (model.problems
                |> Array.get id
                |>Maybe.map
                  (\p -> {p | result = ans, userAnswer = num})
                |> Maybe.map
                  (\p -> Array.set id p model.problems)
                |> Maybe.map (\arr -> {model | problems = arr, index = model.index + 1 })
                |> Maybe.withDefault model
              , Cmd.none
              )

calculate: Int -> String -> Int -> Int
calculate op1 operator op2 =
    let
       answer = if operator == "+" then
          op1 + op2
        else
          op1 * op2

     in
      answer




---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container-fluid"]
        [
        header model
        , body model
--        , debugView model
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
    let
      problem = unwrapProblem <| Array.get(0) model.problems
      hidden = if model.index < 0 then "d-none" else "blah"
    in
     div []
        [
          text "This Problems"
          ,h1 [class hidden] [
              text <| buildProblem problem model.operator
              , input [ value problem.userAnswer, onInput (CheckAnswer 0 problem.answer) ][]
              , text problem.result
            ]
          ]

buildProblem : Problem -> String -> String
buildProblem problem operator =
    renderProblem problem.operands operator


unwrapProblem : Maybe Problem -> Problem
unwrapProblem wrappedProblem =
    case wrappedProblem of
        Just wrappedProblem ->
            wrappedProblem
        Nothing  ->
             { operands = ["" ,  ""], answer = "", userAnswer = "", result = ""}

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

