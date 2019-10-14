port module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder, decodeString, field, int, map2, map3, string)
import Mousikea.Examples.BlueLambda as BlueLambda
import Mousikea.Examples.ChildrenSong6 as ChildrenSong
import Mousikea.Examples.Drums as Drums
import Mousikea.Examples.Mine as Mine
import Mousikea.Examples.SingASongOfSong as SingA
import Mousikea.Generator as Gen
import Mousikea.Midi.MEvent as Perf exposing (Performance)
import Mousikea.Music as Music exposing (..)
import Mousikea.PercussionSound exposing (PercussionSound(..))
import Mousikea.Primitive exposing (Dur, Primitive(..))
import Random
import String
import WebAudioFont



-- JavaScript usage: app.ports.websocketIn.send(response);


port websocketIn : (String -> msg) -> Sub msg



-- JavaScript usage: app.ports.websocketOut.subscribe(handler);


port websocketOut : String -> Cmd msg


port musicStopped : (String -> msg) -> Sub msg


port musicStarted : (String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { static : Dict String Performance
    , random : Dict String (Random.Generator Performance)
    , current : String
    , playing : Maybe Performance
    , nextUp : Maybe Performance
    }


at index list =
    -- 3 [ 1, 2, 3, 4, 5, 6 ]
    if List.length list >= index then
        List.take index list
            -- [ 1, 2, 3 ]
            |> List.reverse
            -- [ 3, 2, 1 ]
            |> List.head
        -- Just 3

    else
        Nothing


v =
    Music.map (\p -> ( p, [ Volume 60 ] ))


fallback =
    Mine.music


available : List Music1
available =
    [ fallback
    , Mine.simpleBeat
    , Mine.simpleBeat2
    , Mine.simpleBeat3
    ]


init : ( Model, Cmd Msg )
init =
    ( { static =
            Dict.empty
                |> Dict.insert "0. Mine" (Mine.music |> Perf.performNote1)
                |> Dict.insert "3. Simple Disco Drum Beat" (Drums.simpleBeat |> Perf.performNote1)
      , random =
            Dict.empty
                |> Dict.insert "5. Randomness with Tonality and Volume" (Gen.randomness |> Random.map Perf.performAbsPitchVol)
                |> Dict.insert "6. Blue Bossa Jam" (Gen.blueBossa |> Random.map Perf.performNote1)
                |> Dict.insert "7. Random Bossa" (Gen.bossa |> Random.map Perf.performNote1)
      , current = "None playing..."
      , playing = Nothing
      , nextUp = Nothing
      }
    , Cmd.none
    )


type Msg
    = Play String
    | Generate String
    | Generated Performance
    | TravisMessage String
    | MusicStarted String
    | MusicStopped String
    | Stop



---- Travis model ----


type alias TravisJobConfig =
    { os : String
    , language : String
    }


travisJobConfigDecoder : Decoder TravisJobConfig
travisJobConfigDecoder =
    map2 TravisJobConfig
        (field "os" string)
        (field "language" string)


type alias TravisJobCommit =
    { sha : String
    , message : String
    }


travisJobCommitDecoder : Decoder TravisJobCommit
travisJobCommitDecoder =
    map2 TravisJobCommit
        (field "sha" string)
        (field "message" string)


type alias TravisJob =
    { id : Int
    , config : TravisJobConfig
    , commit : TravisJobCommit
    }


commitJobDecoder : Decoder TravisJob
commitJobDecoder =
    field "job"
        (map3 TravisJob
            (field "id" int)
            (field "config" travisJobConfigDecoder)
            (field "commit" travisJobCommitDecoder)
        )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ websocketIn TravisMessage
        , musicStopped MusicStopped
        , musicStarted MusicStarted
        ]


nonePlaying =
    "None playing..."



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play key ->
            let
                found =
                    model.static
                        |> Dict.get key
                        |> Maybe.map WebAudioFont.queueWavTable

                nextModel =
                    { model | current = found |> Maybe.map (\m -> "Currently playing: " ++ key) |> Maybe.withDefault nonePlaying }
            in
            ( nextModel, found |> Maybe.withDefault Cmd.none )

        Stop ->
            let
                nextModel =
                    { model | current = nonePlaying }

                nextCmd =
                    Cmd.batch
                        [ WebAudioFont.stop ()

                        -- TODO: update: music stopped
                        ]
            in
            ( nextModel, nextCmd )

        Generate key ->
            let
                found =
                    model.random |> Dict.get key
            in
            ( model, found |> Maybe.map (Random.generate Generated) |> Maybe.withDefault Cmd.none )

        Generated performance ->
            ( model, WebAudioFont.queueWavTable performance )

        TravisMessage message ->
            let
                job =
                    message
                        |> decodeString commitJobDecoder

                nextMessage =
                    job
                        |> Result.map (\j -> j.commit.message)
                        |> Result.withDefault model.current

                music =
                    nextMessage
                        |> String.uncons
                        |> Maybe.map (\( c, rest ) -> Char.toCode c)
                        |> Maybe.withDefault 0
                        |> modBy (List.length available)
                        |> (\i -> at i available)
                        |> Maybe.withDefault fallback

                perf =
                    Par music Mine.simpleBeat
                        |> Perf.performNote1

                ( nextModel, cmd ) =
                    case model.playing |> Debug.log "playing now" of
                        Nothing ->
                            ( { model | nextUp = Nothing, current = nextMessage, playing = Just perf }, WebAudioFont.queueWavTable perf )

                        Just p ->
                            ( { model | nextUp = Just p, current = nextMessage }, Cmd.none )
            in
            ( nextModel, cmd )

        MusicStarted str ->
            let
                playing =
                    model.nextUp |> Maybe.withDefault (Perf.performNote1 fallback)

                nextModel =
                    { model | nextUp = Nothing, playing = Just playing }

                -- TODO:
            in
            str
                |> Debug.log "elm: music started"
                |> (\d -> ( nextModel, Cmd.none ))

        MusicStopped str ->
            let
                nextModel =
                    { model | playing = Nothing }

                toPlay =
                    model.nextUp
                        |> Maybe.withDefault (Perf.performNote1 fallback)

                nextCmd =
                    WebAudioFont.queueWavTable toPlay
            in
            str
                |> Debug.log "elm: music stopped"
                |> (\d -> ( model, nextCmd ))



---- VIEW ----


viewSong : (String -> Msg) -> String -> Html Msg
viewSong msg key =
    Html.div [ Spacing.mt5 ]
        [ Html.h3 [ Spacing.mt5 ] [ Html.text key ]
        , Button.button [ Button.primary, Button.onClick (msg key) ] [ Html.i [ Html.Attributes.class "fas fa-play" ] [] ]
        , Button.button [ Button.attrs [ Spacing.ml2 ], Button.primary, Button.onClick Stop ] [ Html.i [ Html.Attributes.class "fas fa-stop" ] [] ]
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ Html.h1 [] [ Html.text "Henrik & Axel Music Masters" ]
                , model.static
                    |> Dict.keys
                    |> List.map (viewSong Play)
                    |> Html.div []
                , model.random
                    |> Dict.keys
                    |> List.map (viewSong Generate)
                    |> Html.div []
                , Html.h3
                    [ style "margin-top" "2rem" ]
                    [ Html.text model.current ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
