module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, br, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random

---- MODEL ----

type alias Model =
    {
      state : String
      , problems : List Problem
      , random1 : Int
      , random2 : Int
    }

type alias Problem =
    {
      operands : List String
      , operator : String

    }


buildProblemSet : List Problem
buildProblemSet =
    [ {operands = [toString(Random.int 0 9), "456"], operator = "+" }]


init : ( Model, Cmd Msg )
init =
    ( {
        state = "NOT STARTED"
        , problems =  []
        , random1 = 10000
        , random2 = 20000
         }, Cmd.none )

---- UPDATE ----
type Msg
    = START
    | ADD
    | NewRandom1 Int
    | NewRandom2 Int
    | END
    | NoOp



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        START ->
          ( { model | state = "STARTED" }, Random.generate NewRandom1 (Random.int 1 9) )

        NewRandom1 num ->
            ({model | random1 = num} , Random.generate NewRandom2 (Random.int 1 9))

        NewRandom2 num ->
            let
               problem =  { operands = [toString model.random1, toString num], operator = "-" }
               new_problems = problem :: model.problems
            in
              ({model | problems =  new_problems  } , Cmd.none)

        END ->
           ( { model | state = "END", problems = [] }, Cmd.none)

        _ ->
            (model, Cmd.none)



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
        button [onClick END, class "btn btn-outline-primary" ] [text "END"]
        ,button [onClick START, class "btn btn-outline-primary" ] [text "ADD"]
      ]
  ]

body : Model -> Html msg
body model =
   div []
      [
        text "THis Problems"
        , ul [] (List.map (\prob -> li [] [ text (toString prob.operands) ]) model.problems )
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

