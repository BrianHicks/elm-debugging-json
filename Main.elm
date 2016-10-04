module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Events as Events
import Http
import Json.Decode as Decode exposing ((:=))
import Task


type alias User =
    { id : Int
    , name : String
    , username : String
    , email : String
    }


type LoadedUser
    = NotLoaded
    | Loading Int
    | Loaded User
    | Error String


type alias Model =
    { user : LoadedUser
    }


type Msg
    = LoadUser Int
    | UserLoaded User
    | LoadingFailed String



-- Updates


init : ( Model, Cmd Msg )
init =
    { user = NotLoaded } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadUser i ->
            { model | user = Loading i } ! [ loadUser i ]

        UserLoaded user ->
            { model | user = Loaded user } ! []

        LoadingFailed error ->
            { model | user = Error error } ! []



-- Cmds and JSON


userDecoder : Decode.Decoder User
userDecoder =
    Decode.object4
        User
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("username" := Decode.string)
        ("email" := Decode.string)


loadUser : Int -> Cmd Msg
loadUser i =
    let
        url =
            "http://jsonplaceholder.typicode.com/users/" ++ (toString i)
    in
        Task.perform (toString >> LoadingFailed) UserLoaded (Http.get userDecoder url)


fake : String
fake =
    """{
  "id": 1,
  "name": "Leanne Graham",
  "username": "Bret",
  "email": "Sincere@april.biz",
  "address": {
    "street": "Kulas Light",
    "suite": "Apt. 556",
    "city": "Gwenborough",
    "zipcode": "92998-3874",
    "geo": {
      "lat": "-37.3159",
      "lng": "81.1496"
    }
  },
  "phone": "1-770-736-8031 x56442",
  "website": "hildegard.org",
  "company": {
    "name": "Romaguera-Crona",
    "catchPhrase": "Multi-layered client-server neural-net",
    "bs": "harness real-time e-markets"
  }
}"""


loadUserJSON : Int -> Cmd Msg
loadUserJSON _ =
    Decode.decodeString userDecoder fake
        |> Task.fromResult
        |> Task.perform LoadingFailed UserLoaded



-- Loading


loadButton : Int -> Html Msg
loadButton i =
    Html.button [ Events.onClick (LoadUser i) ] [ "load #" ++ (toString i) |> Html.text ]


loadedUserView : LoadedUser -> Html a
loadedUserView user =
    case user of
        NotLoaded ->
            Html.text "not loaded"

        Loading i ->
            "loading #" ++ (toString i) |> Html.text

        Loaded user ->
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

        Error err ->
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


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
