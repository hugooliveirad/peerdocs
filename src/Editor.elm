module Editor exposing (main)

{-| Editor 

@docs main
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
  {}

initModel = {}

-- Update

type Msg
  = None

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    None -> (initModel, Cmd.none)

-- View

view : Model -> Html Msg
view model =
  div [] [text "Editor"]

