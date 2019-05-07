module Type exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Servers = List ServerEntry

type ServerEntry
    = SEData String ServerData
    | SEError String ServerError

type alias ServerError =
    { message: String
    , trace: String
    }

type alias ServerData =
    { time: Posix
    , exec: Float
    , info: ServerInfo
    , player: List String
    }

type alias ServerInfo =
    { hostName: String
    , gameType: String
    , gameName: String
    , version: String
    , map: String
    , players: Int
    , maxPlayers: Int
    , software: String
    }

decodeServers : Decoder Servers
decodeServers = list decodeServerEntry

decodeServerEntry : Decoder ServerEntry
decodeServerEntry = field "server" string 
    |> andThen 
        (\server -> oneOf
            [ field "data" decodeServerData
                |> map (SEData server)
            , field "error" decodeServerError
                |> map (SEError server)
            ]
        )

decodeServerError : Decoder ServerError
decodeServerError = succeed ServerError
    |> required "message" string 
    |> required "trace" string 

decodeServerData : Decoder ServerData 
decodeServerData = succeed ServerData
    |> required "time" (map (Time.millisToPosix << (*) 1000) int)
    |> required "exec" float
    |> required "info" decodeServerInfo
    |> required "player" 
        (oneOf
            [ list string
            , map (always []) bool
            ]
        )

decodeServerInfo : Decoder ServerInfo
decodeServerInfo = succeed ServerInfo
    |> required "HostName" string 
    |> required "GameType" string 
    |> required "GameName" string 
    |> required "Version" string 
    |> required "Map" string 
    |> required "Players" int 
    |> required "MaxPlayers" int 
    |> required "Software" string

decodeMojangUuid : Decoder (String, String)
decodeMojangUuid = map2 Tuple.pair
    (field "id" string)
    (field "name" string)