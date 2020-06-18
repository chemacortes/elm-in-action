module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type alias Url =
    String


type Photo
    = Photo Url



--    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : Url
    , chosenSize : ThumbnailSize
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto Url
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe


initialModel : Model
initialModel =
    { photos =
        [ Photo "1.jpeg"
        , Photo "2.jpeg"
        , Photo "3.jpeg"
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div []
        [ div
            [ class "content" ]
            [ h1 [] [ text "Photo Groove" ] ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl (Photo url) =
    img
        [ src (urlPrefix ++ url)
        , classList [ ( "selected", selectedUrl == url ) ]
        , onClick (ClickedPhoto url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label
        []
        [ input
            [ type_ "radio"
            , name "size"
            , checked (size == chosenSize)
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


getPhotoUrl : Int -> Url
getPhotoUrl index =
    case Array.get index photoArray of
        Just (Photo url) ->
            url

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto photo ->
            ( { model | selectedUrl = photo }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
