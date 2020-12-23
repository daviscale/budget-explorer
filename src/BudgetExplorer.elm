module BudgetExplorer exposing (main)

import Browser
import Chart exposing (hBar, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)


type BudgetDirection
    = Income
    | Expense


budgetDirectionToString : BudgetDirection -> String
budgetDirectionToString budgetDirection =
    case budgetDirection of
        Income ->
            "Income"

        Expense ->
            "Expense"


type alias BudgetCategory =
    { name : String
    , amount : Float
    , direction : BudgetDirection
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
        [ { name = "Mortgage", amount = 2000.0, direction = Expense }
        , { name = "Groceries", amount = 500.0, direction = Expense }
        , { name = "Utilities", amount = 250.0, direction = Expense }
        , { name = "Paycheck", amount = 3500.0, direction = Income }
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
        , htmlForDirection model.data Income
        , htmlForDirection model.data Expense
        ]


htmlForDirection : List BudgetCategory -> BudgetDirection -> Html Msg
htmlForDirection budgetCategories direction =
    let
        categoriesByDirection =
            List.filter (\c -> c.direction == direction) budgetCategories

        budgetDirectionStr =
            budgetDirectionToString direction
    in
    div [ class budgetDirectionStr ] <|
        [ h2 [] [ text <| budgetDirectionStr ++ " Categories" ] ]
            ++ List.map htmlForCategory categoriesByDirection


htmlForCategory : BudgetCategory -> Html Msg
htmlForCategory budgetCategory =
    span []
        [ input [ value budgetCategory.name ] []
        , input [ value <| String.fromFloat budgetCategory.amount ] []
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
