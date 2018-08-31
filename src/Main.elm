module Main exposing (..)

import Color
import Color.Convert
import Html exposing (Html, button, div, h1, h4, img, input, li, span, text, ul)
import Html.Attributes exposing (style)
import Http
import Json.Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Material
import Material.Button as MButton
import Material.Icon as MIcon
import Material.List as MList
import Material.Options as MOptions
import Material.Slider as MSlider
import Material.Textfield as MTextfield


---- MODEL ----


type alias Model =
    { searchInput : String
    , searchResults : SearchResults
    , ownResults : List OwnResult
    , totalTimeWasted : Int
    , mdl :
        Material.Model
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


type alias Mdl =
    Material.Model


init : ( Model, Cmd Msg )
init =
    ( { searchInput = ""
      , searchResults = SearchResults []
      , ownResults = []
      , totalTimeWasted = 0
      , mdl =
            Material.model
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SearchInputChange String
    | AddResultToList SearchItem
    | RemoveFromList OwnResult
    | FetchSearchResult (Result Http.Error SearchResults)
    | FetchWastedTime String (Result Http.Error WastedTime)
    | ChangeTimesWatched OwnResult Float
    | CalculateTotalTimeWasted Int
    | Mdl (Material.Msg Msg)


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
                updateTimesWatched res =
                    if res.item.imdbID == selectectedOwnResult.item.imdbID then
                        { res | timesWatched = round timesWatched }
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

        Mdl msg_ ->
            Material.update Mdl msg_ model


calculateTimeWasted : List OwnResult -> Int
calculateTimeWasted ownList =
    List.map (\r -> r.timesWatched * r.timePerMedium) ownList
        |> List.sum



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Waste of time" ]
        , MTextfield.render Mdl
            [ 2 ]
            model.mdl
            [ MTextfield.label "Search for your favorite Movie/Series"
            , MTextfield.floatingLabel
            , MTextfield.text_
            , MOptions.onInput SearchInputChange
            ]
            []
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
        [ h4 [] [ text "List of Watched Series" ]
        , ul []
            (List.map (showOwnItems model.mdl) model.ownResults)
        , span
            [ style
                [ ( "color", Color.Convert.colorToCssRgb (Color.rgb 212 106 106) )
                , ( "fontSize", "32px" )
                ]
            ]
            [ text ("Total time wasted " ++ toString (minutesToHours model.totalTimeWasted) ++ " h") ]
        ]


minutesToHours : Int -> Int
minutesToHours minutes =
    minutes // 60


displaySearchList : Model -> Html Msg
displaySearchList model =
    ul []
        (List.map (\item -> searchItem model.mdl item (isInOwnList item model.ownResults)) model.searchResults.search)


isInOwnList : SearchItem -> List OwnResult -> Bool
isInOwnList item ownResults =
    List.map (\r -> r.item) ownResults
        |> List.member item


searchItem : Mdl -> SearchItem -> Bool -> Html Msg
searchItem mdl item isDisabled =
    MList.li []
        [ MList.content
            []
            [ --            img [ src item.poster ] [],
              span [] [ text item.title ]
            ]
        , MButton.render Mdl [ 0 ] mdl [ MButton.minifab, MOptions.onClick (AddResultToList item), MOptions.disabled isDisabled ] [ MIcon.i "add" ]
        ]


showOwnItems : Mdl -> OwnResult -> Html Msg
showOwnItems mdl ownResult =
    MList.li []
        [ MList.content
            []
            [ --            img [ src item.poster ] [],
              span [] [ text ownResult.item.title ]
            , MSlider.view
                [ MSlider.onChange (ChangeTimesWatched ownResult)
                , MSlider.value (toFloat ownResult.timesWatched)
                , MSlider.max 10
                , MSlider.step 1
                ]
            , span [] [ text (toString ownResult.timesWatched) ]
            ]
        , MButton.render Mdl [ 0 ] mdl [ MButton.minifab, MOptions.onClick (RemoveFromList ownResult) ] [ MIcon.i "remove" ]
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
