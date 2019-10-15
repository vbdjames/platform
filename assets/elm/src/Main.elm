module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL


type alias Model =
    { gamesList : List Game
    }

type alias Game =
    { title : String
    , description : String
    }

initialModel : Model
initialModel =
    { gamesList = []
    }

initialCommand : Cmd Msg
initialCommand =
    fetchGamesList

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )


-- API


fetchGamesList : Cmd Msg
fetchGamesList =
    Http.get
        { url = "/api/games"
        , expect = Http.expectJson FetchGamesList decodeGamesList
        }

decodeGamesList : Decode.Decoder (List Game)
decodeGamesList =
    decodeGame
        |> Decode.list
        |> Decode.at [ "data" ]

decodeGame : Decode.Decoder Game
decodeGame =
    Decode.map2 Game
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)



-- UPDATE

type Msg 
    = FetchGamesList (Result Http.Error (List Game))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        FetchGamesList result ->
            case result of
                Ok games ->
                    ( { model | gamesList = games }, Cmd.none )

                Err _ ->
                    Debug.log "Error fetching games from API."
                        ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    if List.isEmpty model.gamesList then
        div [] []
    else
        div []
        [ h1 [ class "games-section" ] [ text "Games" ]
        , gamesIndex model
        ]

gamesIndex : Model -> Html msg
gamesIndex model = 
    div [ class "games-index" ] [ gamesList model.gamesList ]

gamesList : List Game -> Html msg
gamesList games = 
    ul [ class "games-list" ] (List.map gamesListItem games)

gamesListItem : Game -> Html msg
gamesListItem game =
    li [ class "game-item" ]
       [ strong [] [text game.title ]
       , p [] [ text game.description ]
       ]
