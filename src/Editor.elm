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
import Peer
import Platform.Cmd exposing ((!))


apiKey =
    "qdr1ywu2uofos9k9"


{-| -}
main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
    , id : String
    , peer : String
    }


initModel =
    { diff = ""
    , text = ""
    , logoot = L.empty ""
    , site = 0
    , clock = 0
    , id = ""
    , peer = ""
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Peer.subscribe PeerMessage



-- Update


type Msg
    = SetSite Int
    | ChangeValue String
    | ChangePeer String
    | PeerMessage Peer.PeerOperation


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetSite site ->
            setSite site model

        ChangeValue text ->
            changeValue text model

        ChangePeer peer ->
            ( { model | peer = peer }, Cmd.none )

        PeerMessage msg ->
            peerMessage msg model


peerMessage : Peer.PeerOperation -> Model -> ( Model, Cmd Msg )
peerMessage { id, operation, pid, content } model =
    let
        modify =
            case operation of
                "insert" ->
                    L.insert pid content

                "remove" ->
                    L.remove pid content

                _ ->
                    identity

        newLogoot =
            modify model.logoot
    in
        ( { model
            | logoot = newLogoot
            , text = newLogoot |> L.toList |> map snd |> foldl (\c str -> str ++ c) ""
          }
        , Cmd.none
        )


setSite : Int -> Model -> ( Model, Cmd Msg )
setSite site model =
    let
        id =
            toString site
    in
        ( { model | site = site, id = toString site }
        , Peer.init { id = id, key = apiKey }
        )


changeValue : String -> Model -> ( Model, Cmd Msg )
changeValue text model =
    let
        diff =
            Diff.diffChars model.text text

        ( newLogoot, peerOperations, clock ) =
            diff |> changesToOperations |> applyOperations model.site model.clock model.logoot
    in
        { model
            | text = newLogoot |> L.toList |> map snd |> foldl (\c str -> str ++ c) ""
            , diff = toString diff
            , logoot = newLogoot
            , clock = clock
        }
            ! Debug.log "wat"
                (peerOperations
                    |> List.map
                        (\( operation, pid, content ) ->
                            Peer.send
                                { id = model.id
                                , key = apiKey
                                , peerId = model.peer
                                , payload =
                                    { id = model.id
                                    , operation = operation
                                    , pid = pid
                                    , content = content
                                    }
                                }
                        )
                )


type Operation
    = Insert Char
    | Remove Char
    | Noop Int


type alias PeerOperation =
    ( String, L.Pid, String )


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


applyOperations : L.Site -> L.Clock -> L.Logoot String -> List Operation -> ( L.Logoot String, List PeerOperation, L.Clock )
applyOperations site clock logoot ops =
    let
        ( _, newLogoot, peerOperations, newClock ) =
            foldl (applyOperation site) ( 0, logoot, [], clock ) ops
    in
        ( newLogoot, peerOperations, newClock )


applyOperation : L.Site -> Operation -> ( Int, L.Logoot String, List PeerOperation, L.Clock ) -> ( Int, L.Logoot String, List PeerOperation, L.Clock )
applyOperation site op ( cursor, logoot, peerOperations, clock ) =
    let
        pidDefault =
            Maybe.withDefault ( [ ( 0, 0 ) ], 0 )

        ( newCursor, newLogoot, newOperations, newClock ) =
            case op of
                Insert char ->
                    let
                        content =
                            (String.fromChar char)

                        new =
                            Maybe.withDefault logoot <|
                                L.insertAt site clock cursor content logoot
                    in
                        ( cursor + 1
                        , new
                        , ( "insert", (pidAtIndex (cursor + 1) new) |> pidDefault, content ) :: peerOperations
                        , clock + 1
                        )

                Remove char ->
                    let
                        pid =
                            (pidAtIndex (cursor + 1) logoot |> pidDefault)

                        content =
                            (String.fromChar char)

                        new =
                            L.remove pid (String.fromChar char) logoot
                    in
                        ( cursor
                        , new
                        , ( "remove", pid, content ) :: peerOperations
                        , clock
                        )

                Noop step ->
                    ( cursor + step
                    , logoot
                    , peerOperations
                    , clock
                    )
    in
        ( newCursor, newLogoot, newOperations, newClock )


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
        , div []
            [ label []
                [ text "Peer "
                , input [ value model.peer, onInput ChangePeer ] []
                ]
            ]
        , div [] [ text ("Your id is:" ++ model.id) ]
        , pre [] [ text model.diff ]
        , pre [ style [ ( "whiteSpace", "preWrap" ) ] ] [ model.logoot |> L.toList |> toString |> text ]
        ]
