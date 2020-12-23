module BudgetExplorer exposing (main)

import Browser
import Chart exposing (hBar, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { data : List ( Float, String )
    }


initialModel : Model
initialModel =
    { data =
        [ ( 1000.0, "Mortgage" )
        , ( 500.0, "Groceries" )
        , ( 250.0, "Utilities" )
        ]
    }


type Msg
    = CategoryUpdated String Float


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Budget Explorer" ]
        , hBar model.data
            |> Chart.title "here we go"
            |> toHtml
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
