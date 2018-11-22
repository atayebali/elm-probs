module Utils exposing (..)
import Array exposing (..)

fromJust : Maybe String -> String
fromJust maybe =
    case maybe of
        Nothing ->
          "0"
        Just value ->
          value

renderProblem : List String -> String -> String
renderProblem ops operator =
    let
      op1 = List.head ops
      op2 = ops
             |> List.reverse
             |> List.head
    in
      Maybe.withDefault "0" op1  ++
      operator ++
      Maybe.withDefault "0" op2 ++
      " = "

happy : String -> String
happy idx =
    let
        idx_num = Result.withDefault 0 (String.toInt idx)
        mojis = Array.fromList ["😀", "😍"]
        emo_dex = idx_num % Array.length mojis
    in
     Maybe.withDefault " " (Array.get(emo_dex) mojis)

sad : String -> String
sad idx =
    let
        idx_num = Result.withDefault 0 (String.toInt idx)
        mojis = Array.fromList ["🤮", "🤬"]
        emo_dex = idx_num % Array.length mojis
    in
     Maybe.withDefault " " (Array.get(emo_dex) mojis)
