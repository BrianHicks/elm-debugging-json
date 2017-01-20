module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (field)
import RemoteData exposing (WebData)


type alias User =
    { id : Int
    , name : String
    , username : String
    , email : String
    }


type alias Model =
    { user : WebData User
    }


type Msg
    = LoadUser Int
    | UserLoaded (WebData User)



-- Updates


init : ( Model, Cmd Msg )
init =
    { user = RemoteData.NotAsked } ! [ loadUser 1 ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserLoaded user ->
            { model | user = user } ! []

        LoadUser i ->
            { model | user = RemoteData.Loading } ! [ loadUser i ]



-- Cmds and JSON


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4
        User
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "username" Decode.string)
        (field "email" Decode.string)


loadUser : Int -> Cmd Msg
loadUser i =
    Http.get ("https://jsonplaceholder.typicode.com/users/" ++ toString i) userDecoder
        |> RemoteData.sendRequest
        |> Cmd.map UserLoaded



-- Loading


loadButton : Int -> Html Msg
loadButton i =
    Html.button [ Events.onClick (LoadUser i) ] [ "load #" ++ (toString i) |> Html.text ]


loadedUserView : WebData User -> Html a
loadedUserView user =
    case user of
        RemoteData.NotAsked ->
            Html.text "not loaded"

        RemoteData.Loading ->
            Html.text "loading"

        RemoteData.Success user ->
            Html.table
                []
                [ Html.thead []
                    [ Html.tr
                        []
                        (List.map
                            (\h -> Html.th [] [ Html.text h ])
                            [ "ID", "Name", "Username", "Email" ]
                        )
                    ]
                , Html.tbody []
                    [ Html.tr
                        []
                        (List.map
                            (\content -> Html.td [] [ Html.text content ])
                            [ user.id |> toString
                            , user.name
                            , user.username
                            , user.email
                            ]
                        )
                    ]
                ]

        RemoteData.Failure err ->
            "error! " ++ (toString err) |> Html.text


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            (List.map loadButton [ 1, 2, 3, 4, 5 ])
        , loadedUserView model.user
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
