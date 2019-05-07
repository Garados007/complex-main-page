module Network exposing 
    ( NetworkMsg
    , Return (..)
    , update
    , send
    , sendId
    )

import Type exposing (Servers, decodeServers, decodeMojangUuid)

import Http exposing (Error)
import HttpBuilder exposing (withTimeout, withExpect)
import Time exposing (Posix)
import Result exposing (Result)
import Url.Builder as UrlBuilder

type NetworkMsg
    = Fetch (Result Error Servers)
    | NFetch (Result Error (String, String))

type Return 
    = None
    | RServers Servers
    | RMojang (String, String)

send : Posix -> Cmd NetworkMsg
send time = HttpBuilder.get 
        ( "http://complex.mabron.de/mc/data/server.json?time=" 
            ++ (String.fromInt <| Time.posixToMillis time)
        )
    |> withTimeout 10000
    |> withExpect (Http.expectJson Fetch decodeServers)
    |> HttpBuilder.request

sendId : String -> Cmd NetworkMsg
sendId name = UrlBuilder.crossOrigin "https://api.minetools.eu"
        [ "uuid", name ] []
    |> HttpBuilder.get
    |> withTimeout 10000
    |> withExpect (Http.expectJson NFetch decodeMojangUuid)
    |> HttpBuilder.request
        
update : NetworkMsg -> (Cmd NetworkMsg, Return)
update msg =
    case msg of
        Fetch (Ok resp) ->
            ( Cmd.none
            , RServers resp
            )
        Fetch (Err err) ->
            let
                debug = Debug.log "Network Error" err
            in
                ( Cmd.none
                , None
                )
        NFetch (Ok resp) ->
            ( Cmd.none
            , RMojang resp
            )
        NFetch (Err err) ->
            let
                debug = Debug.log "Network Error" err
            in
                ( Cmd.none
                , None
                )
