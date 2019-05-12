module Page exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Network exposing (Return (..))
import Type exposing (..)
import Browser
import Time exposing (Posix)
import Task
import Dict exposing (Dict)
import Set exposing (Set)

type alias Model =
    { servers: Servers
    , selected: String
    , now: Posix
    , ids: Dict String String
    }

type Msg 
    = WrapNetwork Network.NetworkMsg
    | NewTime Posix
    | SetSelected String

-- main = text <| Debug.toString testResult

main = Browser.document
    { init = \() -> init
    , view = Browser.Document "2 Complex" << List.singleton << view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = 
    ( Model [] "" (Time.millisToPosix 0) Dict.empty
    , Task.perform NewTime Time.now
    )

view : Model -> Html Msg
view model = div []
    [ div [ class "container title" ]
        [ div [ class "text" ]
            [ text "Server Status" ]
        , a
            [ class "discord"
            , href "https://discord.gg/5bha8ka"
            , target "_blank"
            ]
            [ text "Discord" ]
        ]
    , div [ class "container stats" ]
        <| List.map 
            (\server -> 
                let diff : Posix -> Posix -> Int 
                    diff st ct = (Time.posixToMillis ct) - (Time.posixToMillis st)
                    cat : Int -> Int
                    cat timeDiff = 
                        if timeDiff < 30000
                        then 0
                        else if timeDiff < 90000
                        then 1
                        else 2
                    (name, stat) = case server of 
                        SEData n d ->
                            ( n 
                            , cat <| diff d.time model.now
                            )
                        SEError n _ -> ( n, 2 )
                    lamp : Int -> String 
                    lamp index = if index == stat 
                        then "lamp active lamp-" ++ (String.fromInt index)
                        else "lamp lamp-" ++ (String.fromInt index)
                in div [ class "stat" ]
                    [ div [ class "name" ]
                        [ text name ]
                    , div [ class "lamps" ]
                        [ div [ class <| lamp 2 ] []
                        , div [ class <| lamp 1 ] []
                        , div [ class <| lamp 0 ] []
                        ]
                    ]
            )
        <| model.servers
    , div [ class "container vert-diff" ]
        [ viewInfoSection model
        , viewPlayerSection model
        ]
    , stylesheet "/css/style.css"
    ]

viewInfoSection : Model -> Html Msg 
viewInfoSection model = div [ class "info-section" ]
    [ div [ class "tabs" ]
        <| List.map
            (\name -> div 
                [ class 
                    <| (++) "tab"
                    <| if name == model.selected
                        then " active"
                        else ""
                , onClick <| SetSelected name
                ]
                [ text name]
            )
        <| List.map
            (\entry -> case entry of 
                SEData name _ -> name
                SEError name _ -> name
            )
        <| model.servers
    , filterOrHead 
        (\entry -> case entry of 
            SEData name _ -> name == model.selected
            SEError name _ -> name == model.selected
        )
        model.servers
        |> \d -> case d of 
            Nothing -> div [ class "content empty" ]
                [ text "Keine Server abgerufen!" ]
            Just (SEData _ data) -> table [ class "content grid" ]
                <| List.map 
                    (\(key,value) -> tr []
                        [ th [] [ text key ]
                        , td [] [ text value ]
                        ]
                    )
                <|  [ ("Hostname: ", data.info.hostName)
                    -- , ("Game Type: ", data.info.gameType)
                    -- , ("Game Name: ", data.info.gameName)
                    , ("Version: ", data.info.version)
                    -- , ("Map: ", data.info.map)
                    , ( "Players: "
                        , (String.fromInt data.info.players)
                            ++ " / "
                            ++ (String.fromInt data.info.maxPlayers)
                        )
                    , ("Software: ", data.info.software)
                    ]
            Just (SEError _ err) -> div [ class "content error" ]
                [ div [ class "message" ]
                    [ text err.message ]
                , div [ class "trace" ]
                    <| List.map (\t -> div [] [ text t ])
                    <| String.split "\n"
                    <| err.trace
                ]

    ]

viewPlayerSection : Model -> Html Msg
viewPlayerSection model =
    let (p1, p2) = Dict.partition
            (\_ -> Set.member model.selected)
            <| getPlayerData model.servers
        viewPlayer : String -> Set String -> Html Msg 
        viewPlayer player servers = tr
            [ class "player" ]
            [ td []
                <| case Dict.get player model.ids of
                    Nothing -> []
                    Just id -> List.singleton
                        <| img [ src <| "https://crafatar.com/avatars/" ++ id ]
                            []
            , td []
                [ div [ class "name" ]
                    [ text player ]
                , div [ class "servers" ]
                    <| List.map 
                        (\s -> div [ class "server" ] [ text s ])
                    <| Set.toList servers
                ]
            ]
        uncurry func (a, b) = func a b
        header : Html Msg 
        header = tr []
            [ th [ colspan 2 ]
                [ text "Spieler" ]
            ]
    in table 
        [ class "player-section" ]
        <| header
        :: (List.map (uncurry viewPlayer) <| Dict.toList p1)
        ++ (List.map (uncurry viewPlayer) <| Dict.toList p2)
    

getPlayerData : Servers -> Dict String (Set String)
getPlayerData = List.foldl
    (\entry d1 -> case entry of 
        SEError _ _ -> d1 
        SEData name data -> List.foldl 
            (\player d2 -> Dict.get player d2
                |> Maybe.withDefault Set.empty
                |> Set.insert name
                |> \s -> Dict.insert player s d2
            )
            d1
            data.player
    )
    Dict.empty

filterOrHead : (a -> Bool) -> List a -> Maybe a 
filterOrHead func list = 
    case List.filter func list 
        |> List.head 
    of 
        Just h -> Just h 
        Nothing -> List.head list 

stylesheet : String -> Html msg
stylesheet url = node "link"
    [ attribute "rel" "stylesheet"
    , attribute "property" "stylesheet"
    , attribute "href" url
    ] []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    WrapNetwork smsg ->
        let (scmd, res) = Network.update smsg
            newUser = case res of 
                RServers s -> List.concatMap
                    (\entry -> case entry of 
                        SEError _ _ -> []
                        SEData _ d -> d.player
                    )
                    s
                    |> Set.fromList
                    |> Set.filter
                        (\p -> not <| Dict.member p model.ids)
                    |> Set.toList
                _ -> []
            servers = case res of 
                RServers s -> List.map
                    (\entry -> case entry of 
                        SEError name _ -> name 
                        SEData name _ -> name 
                    )
                    s
                _ -> []
        in  Tuple.pair
            ( case res of 
                None -> model 
                RServers s -> 
                    { model 
                    | servers = s 
                    , selected =
                        if List.member model.selected servers 
                        then model.selected 
                        else Maybe.withDefault "" <| List.head servers
                    }
                RMojang (id, name) ->
                    { model | ids = Dict.insert name id model.ids }
            )
            <| Cmd.map WrapNetwork
            <| Cmd.batch
            <| (::) scmd
            <| List.map (Network.sendId)
            <| newUser
    NewTime now ->
        ( { model | now = now }
        , Cmd.map WrapNetwork <| Network.send now
        )
    SetSelected sel ->
        ( { model | selected = sel }
        , Cmd.none 
        )

subscriptions : Model -> Sub Msg 
subscriptions model = Time.every 10000 NewTime
