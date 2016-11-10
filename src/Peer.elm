port module Peer exposing (..)

import Logoot as L


type alias InitOptions =
    { id : String
    , key : String
    }


type alias SendOptions =
    { id : String
    , key : String
    , peerId : String
    , payload : PeerOperation
    }


type alias PeerOperation =
    { id : String
    , operation : String
    , pid : L.Pid
    , content : String
    }


type alias PeerID =
    { id : String }


port init : InitOptions -> Cmd msg


port send : SendOptions -> Cmd msg


port subscribe : (PeerOperation -> msg) -> Sub msg
