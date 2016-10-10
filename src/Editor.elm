module Editor exposing (main)

{-| Editor 

@docs main
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Diff
import String
import Maybe
import Logoot as L
import List exposing (..)

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
  , logoot : L.Logoot
  }

initModel =
  { diff = ""
  , text = ""
  , logoot = L.empty
  }

-- Update

type Msg
  = ChangeValue String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ChangeValue text -> changeValue text model

changeValue : String -> Model -> (Model, Cmd Msg) 
changeValue text model =
  let
    diff = Diff.diffChars model.text text
  in
  ({model
  | text = text
  , diff = toString diff
  , logoot = diff |> changesToOperations |> applyOperations model.logoot
  }, Cmd.none)

type Operation
  = Insert Char
  | Remove Char
  | Noop Char

changesToOperations : List Diff.Change -> List Operation
changesToOperations = concatMap changeToOperations

changeToOperations : Diff.Change -> List Operation
changeToOperations change =
  case change of
    Diff.Changed rem add -> concat 
      [ Diff.Removed rem |> changeToOperations
      , Diff.Added add |> changeToOperations
      ]
    Diff.NoChange str -> String.foldr (\c -> Noop c |> (::)) [] str
    Diff.Removed str -> String.foldr (\c -> Remove c |> (::)) [] str
    Diff.Added str -> String.foldr (\c -> Insert c |> (::)) [] str

applyOperations : L.Logoot -> List Operation -> L.Logoot
applyOperations logoot = snd << foldl applyOperation (0, logoot)

applyOperation : Operation -> (Int, L.Logoot) -> (Int, L.Logoot)
applyOperation op (cursor, logoot) =
  let
    pidDefault = Maybe.withDefault ([(0,0)],0)
  in
  case op of
    Insert char ->
      (cursor + 1, L.insertAfter 0 0 (toString char) (pidAtIndex cursor logoot |> pidDefault) logoot)
    Remove char ->
      (cursor, L.remove (pidAtIndex (cursor + 1) logoot |> pidDefault) (toString char) logoot)
    Noop char -> (cursor + 1, logoot)

pidAtIndex : Int -> L.Logoot -> Maybe L.Pid
pidAtIndex index logoot =
  logoot
    |> L.toList
    |> indexedMap (,)
    |> filter (fst >> (==) index)
    |> map snd
    |> map fst
    |> head

-- View

view : Model -> Html Msg
view model =
  div []
    [ textarea [value model.text, onInput ChangeValue] []
    , pre [] [text model.diff]
    , pre [] [model.logoot |> L.toList |> toString |> text]
    ]

