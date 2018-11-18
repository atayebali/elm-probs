module Utils exposing (..)

fromJust : Maybe String -> String
fromJust maybe =
    case maybe of
        Nothing ->
          "0"
        Just value ->
          value

renderProblem : List String -> String
renderProblem ops =
    let
      op1 = List.head ops
      op2 = ops
             |> List.reverse
             |> List.head

    in
      fromJust op1  ++
      " * " ++
       fromJust op2 ++
       " = "