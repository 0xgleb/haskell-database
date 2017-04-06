import Html exposing (beginnerProgram, div, button, h1, text)
import Html.Events exposing (onClick)

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment -> model + 1
    Decrement -> model - 1

view model = 
  div [] 
    [ h1 [] [ text "Welcome to Haskell-DB!" ]
    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

main = 
  beginnerProgram { model = 0, view = view, update = update }
