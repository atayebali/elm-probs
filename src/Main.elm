module Main exposing (..)

import Html exposing (Html, text, div, h1, button, p, br)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random

---- MODEL ----


type alias Model =
    {
      state : String
      , problems : ProblemSet
      , random : Int
    }

type alias ProblemSet =
    { set : List Problem }

type alias Problem =
    {
      operands : List String
      , operator : String

    }

initProblemSet : ProblemSet
initProblemSet =
    {
       set = buildProblemSet
    }

buildProblemSet : List Problem
buildProblemSet =
    [ {operands = [toString(Random.int 0 9), "456"], operator = "+" }]


init : ( Model, Cmd Msg )
init =
    ( {
        state = "NOT STARTED"
        , problems =  initProblemSet
        , random = 10000
         }, Cmd.none )



---- UPDATE ----


type Msg
    = START
    | NewRandom Int
    | END
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        START ->
          ( { model | state = "STARTED" }, Random.generate NewRandom (Random.int 1 9) )

        NewRandom num ->
            ({model | random = num} , Cmd.none)

        END ->
           ( { model | state = "END" }, Cmd.none)

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
      [ button [onClick END, class "btn btn-outline-primary" ] [text "END"] ]
  ]

body : Model -> Html Msg
body model =
   div [] []

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

