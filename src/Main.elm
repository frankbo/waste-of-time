module Main exposing (..)

import Html exposing (Html, button, div, h1, img, input, li, span, text, ul)
import Html.Attributes exposing (disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


---- MODEL ----


type alias Model =
    { searchInput : String
    , searchResults : SearchResults
    , ownResults : List OwnResult
    , totalTimeWasted : Int
    }


type alias OwnResult =
    { item : SearchItem
    , timesWatched : Int
    , timePerMedium : Int
    }


type alias SearchResults =
    { search : List SearchItem
    }


type alias SearchItem =
    { title : String
    , imdbID : String
    , poster : String
    }


type alias WastedTime =
    { timeInMinutes : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { searchInput = "", searchResults = SearchResults [], ownResults = [], totalTimeWasted = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = SearchInputChange String
    | AddResultToList SearchItem
    | RemoveFromList OwnResult
    | FetchSearchResult (Result Http.Error SearchResults)
    | FetchWastedTime String (Result Http.Error WastedTime)
    | ChangeTimesWatched OwnResult String
    | CalculateTotalTimeWasted Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChange value ->
            ( { model | searchInput = value }, fetchSearchResult value )

        AddResultToList searchItem ->
            ( { model | ownResults = OwnResult searchItem 0 1 :: model.ownResults }, fetchTotalTimeWastedInMinutes searchItem.imdbID )

        RemoveFromList ownResult ->
            let
                newOwnResults =
                    List.filter (\res -> res.item.imdbID /= ownResult.item.imdbID) model.ownResults
            in
            ( { model | ownResults = newOwnResults, totalTimeWasted = calculateTimeWasted newOwnResults }, Cmd.none )

        ChangeTimesWatched selectectedOwnResult timesWatched ->
            let
                toInt watched =
                    case String.toInt watched of
                        Ok v ->
                            v

                        Err message ->
                            0

                updateTimesWatched res =
                    if res.item.imdbID == selectectedOwnResult.item.imdbID then
                        { res | timesWatched = toInt timesWatched }
                    else
                        res

                newOwnResults =
                    List.map updateTimesWatched model.ownResults
            in
            ( { model | ownResults = newOwnResults, totalTimeWasted = calculateTimeWasted newOwnResults }, Cmd.none )

        CalculateTotalTimeWasted totalTimeWasted ->
            ( { model | totalTimeWasted = totalTimeWasted }, Cmd.none )

        FetchSearchResult (Ok res) ->
            ( { model | searchResults = res }, Cmd.none )

        FetchSearchResult (Err _) ->
            ( model, Cmd.none )

        FetchWastedTime imdbID (Ok res) ->
            let
                updateTimesPerMedium item =
                    if item.item.imdbID == imdbID then
                        { item | timePerMedium = res.timeInMinutes }
                    else
                        item

                newOwnResults =
                    List.map updateTimesPerMedium model.ownResults
            in
            ( { model | ownResults = newOwnResults, totalTimeWasted = calculateTimeWasted newOwnResults }, Cmd.none )

        FetchWastedTime imdbID (Err _) ->
            ( model, Cmd.none )


calculateTimeWasted : List OwnResult -> Int
calculateTimeWasted ownList =
    List.map (\r -> r.timesWatched * r.timePerMedium) ownList
        |> List.sum



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Waste of time" ]
        , input [ placeholder "Search for your favorite Movie/Series", onInput SearchInputChange ] []
        , searchResults model
        ]


searchResults : Model -> Html Msg
searchResults model =
    if String.isEmpty model.searchInput && List.isEmpty model.searchResults.search then
        div [] [ text "Nothing found here. Try again!!!" ]
    else
        div []
            [ displaySearchList model
            , displayOwnList model
            ]


displayOwnList : Model -> Html Msg
displayOwnList model =
    div []
        [ ul []
            (List.map showOwnItems model.ownResults)
        , span [] [ text ("Total time wasted " ++ toString (minutesToHours model.totalTimeWasted) ++ " h") ]
        ]


minutesToHours : Int -> Int
minutesToHours minutes =
    minutes // 0


displaySearchList : Model -> Html Msg
displaySearchList model =
    ul []
        (List.map (\item -> searchItem item (isInOwnList item model.ownResults)) model.searchResults.search)


isInOwnList : SearchItem -> List OwnResult -> Bool
isInOwnList item ownResults =
    List.map (\r -> r.item) ownResults
        |> List.member item


searchItem : SearchItem -> Bool -> Html Msg
searchItem item isDisabled =
    li []
        [ --            img [ src item.poster ] [],
          span [] [ text item.title ]
        , button [ onClick (AddResultToList item), disabled isDisabled ] [ text "+" ]
        ]


showOwnItems : OwnResult -> Html Msg
showOwnItems ownResult =
    li []
        [ span [] [ text ownResult.item.title ]
        , input [ type_ "number", onInput (ChangeTimesWatched ownResult), value (toString ownResult.timesWatched) ] []
        , button [ onClick (RemoveFromList ownResult) ] [ text "-" ]
        ]



---- HTTP ----


fetchSearchResult : String -> Cmd Msg
fetchSearchResult searchTerm =
    let
        baseUrl =
            "http://localhost:8080"

        searchUrl =
            baseUrl ++ "/search?q=" ++ searchTerm

        request =
            Http.get searchUrl decodeSearchTerms
    in
    Http.send FetchSearchResult request


fetchTotalTimeWastedInMinutes : String -> Cmd Msg
fetchTotalTimeWastedInMinutes imdbID =
    let
        baseUrl =
            "http://localhost:8080"

        searchUrl =
            baseUrl ++ "/imdb?id=" ++ imdbID

        request =
            Http.get searchUrl decodeWastedTime
    in
    Http.send (FetchWastedTime imdbID) request



---- HTTP DECODER ----


decodeSearchTerms : Json.Decode.Decoder SearchResults
decodeSearchTerms =
    decode SearchResults
        |> required "Search" (list decodeSearchTerm)


decodeSearchTerm : Json.Decode.Decoder SearchItem
decodeSearchTerm =
    decode SearchItem
        |> required "Title" string
        |> required "imdbID" string
        |> required "Poster" string


decodeWastedTime : Json.Decode.Decoder WastedTime
decodeWastedTime =
    decode WastedTime
        |> required "timeInMinutes" int



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
