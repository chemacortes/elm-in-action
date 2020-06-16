module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)


type alias Model =
    List { url : String }


type alias Msg =
    String


initialModel : Model
initialModel =
    [ { url = "1.jpg" }
    , { url = "2.jpg" }
    , { url = "3.jpg" }
    ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "content" ]
            [ h1 [] [ text "Photo Groove" ] ]
        , div [ id "thumbnails" ]
            [ img [ src "http://elm-in-action.com/1.jpeg" ] []
            , img [ src "http://elm-in-action.com/2.jpeg" ] []
            , img [ src "http://elm-in-action.com/3.jpeg" ] []
            ]
        ]


main : Html Msg
main =
    view initialModel
