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


init =
    ( initModel, Cmd.none )



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


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ChangeValue text ->
            changeValue text model


changeValue : String -> Model -> ( Model, Cmd Msg )
changeValue text model =
    let
        diff =
            Diff.diffChars model.text text

        newLogoot =
            diff |> changesToOperations |> applyOperations model.logoot
    in
        ( { model
            | text = newLogoot |> L.toList |> map snd |> foldl (\c str -> str ++ c) ""
            , diff = toString diff
            , logoot = newLogoot
          }
        , Cmd.none
        )


type Operation
    = Insert Char
    | Remove Char
    | Noop Int


changesToOperations : List Diff.Change -> List Operation
changesToOperations =
    concatMap changeToOperations


changeToOperations : Diff.Change -> List Operation
changeToOperations change =
    case change of
        Diff.Changed rem add ->
            concat
                [ Diff.Removed rem |> changeToOperations
                , Diff.Added add |> changeToOperations
                ]

        Diff.NoChange str ->
            [ Noop (String.length str) ]

        Diff.Removed str ->
            String.foldl (\c l -> l ++ [ Remove c ]) [] str

        Diff.Added str ->
            String.foldl (\c l -> l ++ [ Insert c ]) [] str


applyOperations : L.Logoot -> List Operation -> L.Logoot
applyOperations logoot =
    snd << foldl applyOperation ( 0, logoot )


applyOperation : Operation -> ( Int, L.Logoot ) -> ( Int, L.Logoot )
applyOperation op ( cursor, logoot ) =
    let
        pidDefault =
            Maybe.withDefault ( [ ( 0, 0 ) ], 0 )
    in
        case op of
            Insert char ->
                ( cursor + 1, L.insertAfter 0 0 (String.fromChar char) (pidAtIndex cursor logoot |> pidDefault) logoot )

            Remove char ->
                ( cursor, L.remove (pidAtIndex (cursor + 1) logoot |> pidDefault) (String.fromChar char) logoot )

            Noop step ->
                ( cursor + step, logoot )


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
        [ textarea [ style [ ( "width", "90%" ), ( "height", "100px" ) ], value model.text, onInput ChangeValue ] []
        , pre [] [ text model.diff ]
        , pre [ style [ ( "whiteSpace", "preWrap" ) ] ] [ model.logoot |> L.toList |> toString |> text ]
        ]
