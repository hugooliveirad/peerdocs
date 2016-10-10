module Editor exposing (main)

{-| Editor 

@docs main
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Diff

{-| -}
main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- Init

init = (initModel, Cmd.none)

-- Model

type alias Model =
  { diff : String
  , text : String
  }

initModel =
  { diff = ""
  , text = ""
  }

-- Update

type Msg
  = ChangeValue String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ChangeValue text -> ({model | text = text, diff = Diff.diffChars model.text text |> toString}, Cmd.none)

-- View

view : Model -> Html Msg
view model =
  div []
    [ textarea [value model.text, onInput ChangeValue] []
    , pre [] [text model.diff]]

