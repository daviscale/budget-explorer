module BudgetExplorer exposing (main)

import Browser
import Chart exposing (hBar, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias BudgetCategory =
    { name : String
    , amount : Float
    }


budgetCategoryToTuple : BudgetCategory -> ( Float, String )
budgetCategoryToTuple budgetCategory =
    ( budgetCategory.amount, budgetCategory.name )


type alias Model =
    { data : List BudgetCategory
    }


initialModel : Model
initialModel =
    { data =
        [ { name = "Mortgage", amount = 2000.0 }
        , { name = "Groceries", amount = 500.0 }
        , { name = "Utilities", amount = 250.0 }
        ]
    }


type Msg
    = CategoryUpdated BudgetCategory


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Budget Explorer" ]
        , hBar (List.map budgetCategoryToTuple model.data)
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
