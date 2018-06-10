module Main exposing (..)

import Html exposing (Html, text, div, h1, input, ul, li)
import Html.Attributes exposing (src, placeholder   )
import Html.Events exposing (onInput)


---- MODEL ----


type alias Model =
    { searchInput : String}


init : ( Model, Cmd Msg )
init =
    ( { searchInput = ""}, Cmd.none )



---- UPDATE ----


type Msg
    = SearchInputChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    SearchInputChange value ->
        ({model | searchInput = value}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Waste of time"],
          input [placeholder "Search for your favorite Movie/Series", onInput SearchInputChange ] [],
          (searchResults model ["hello", "world"])
        ]


searchResults : Model -> List string -> Html Msg
searchResults model results  =
    if (String.isEmpty model.searchInput) then
        div [] [ text "Nothing found so far. Try again" ]
    else
        div [] [
            ul [] [
                li [] [text "test"]
            ]
        ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
