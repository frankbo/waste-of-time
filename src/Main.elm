module Main exposing (..)

import Html exposing (Html, text, div, h1, input, ul, li)
import Html.Attributes exposing (src, placeholder   )
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (int, string, float, nullable, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


---- MODEL ----


type alias Model =
    { searchInput : String,
      searchResults : SearchResults
    }

type alias SearchResults =
    { search: List SearchItem
    }

type alias SearchItem =
    { title : String
    }


init : ( Model, Cmd Msg )
init =
    ( { searchInput = "", searchResults = (SearchResults [])}, Cmd.none )



---- UPDATE ----


type Msg
    = SearchInputChange String
    | FetchSearchResult (Result Http.Error SearchResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    SearchInputChange value ->
        ({ model | searchInput = value }, fetchSearchResult value)

    FetchSearchResult (Ok res) ->
        ( { model | searchResults = res }, Cmd.none)

    FetchSearchResult (Err _) ->
        (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Waste of time" ],
          input [placeholder "Search for your favorite Movie/Series", onInput SearchInputChange ] [],
          (searchResults model)
        ]


searchResults : Model -> Html Msg
searchResults model  =
    if ((String.isEmpty model.searchInput) && (List.isEmpty model.searchResults.search)) then
        div [] [ text "Nothing found so far. Try again" ]
    else
        div [] [
            ul []
                (List.map searchItem model.searchResults.search)

        ]

searchItem : SearchItem -> Html Msg
searchItem item =
    li [] [text item.title]

---- HTTP ----

fetchSearchResult : String -> Cmd Msg
fetchSearchResult searchTerm =
    let
        apiKey = "454bff4d"
        url =
          "http://www.omdbapi.com/?apikey=" ++ apiKey ++ "&s=" ++ searchTerm

        request =
          Http.get url decodeSearchTerms
    in
        Http.send FetchSearchResult request

---- HTTP DECODER ----

decodeSearchTerms : Json.Decode.Decoder SearchResults
decodeSearchTerms =
    decode SearchResults
        |> required "Search" (list decodeSearchTerm)

decodeSearchTerm : Json.Decode.Decoder SearchItem
decodeSearchTerm =
    decode SearchItem
        |> required "Title" string

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
