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
import Random


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
    ( initModel, Random.generate SetSite (Random.int 1 32000) )



-- Model


type alias Model =
    { diff : String
    , text : String
    , logoot : L.Logoot String
    , site : L.Site
    , clock : L.Clock
    }


initModel =
    { diff = ""
    , text = ""
    , logoot = L.empty ""
    , site = 0
    , clock = 0
    }



-- Update


type Msg
    = SetSite Int
    | ChangeValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetSite site ->
            ( { model | site = site }, Cmd.none )

        ChangeValue text ->
            changeValue text model


changeValue : String -> Model -> ( Model, Cmd Msg )
changeValue text model =
    let
        diff =
            Diff.diffChars model.text text

        ( newLogoot, clock ) =
            diff |> changesToOperations |> applyOperations model.site model.clock model.logoot
    in
        ( { model
            | text = newLogoot |> L.toList |> map snd |> foldl (\c str -> str ++ c) ""
            , diff = toString diff
            , logoot = newLogoot
            , clock = clock
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


applyOperations : L.Site -> L.Clock -> L.Logoot String -> List Operation -> ( L.Logoot String, L.Clock )
applyOperations site clock logoot ops =
    let
        ( _, newLogoot, newClock ) =
            foldl (applyOperation site) ( 0, logoot, clock ) ops
    in
        ( newLogoot, newClock )


applyOperation : L.Site -> Operation -> ( Int, L.Logoot String, L.Clock ) -> ( Int, L.Logoot String, L.Clock )
applyOperation site op ( cursor, logoot, clock ) =
    let
        pidDefault =
            Maybe.withDefault ( [ ( 0, 0 ) ], 0 )

        ( newCursor, newLogoot, newClock ) =
            case op of
                Insert char ->
                    ( cursor + 1, Maybe.withDefault logoot <| L.insertAt site clock cursor (String.fromChar char) logoot, clock + 1 )

                Remove char ->
                    ( cursor, L.remove (pidAtIndex (cursor + 1) logoot |> pidDefault) (String.fromChar char) logoot, clock )

                Noop step ->
                    ( cursor + step, logoot, clock )
    in
        ( newCursor, newLogoot, newClock )


pidAtIndex : Int -> L.Logoot String -> Maybe L.Pid
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
