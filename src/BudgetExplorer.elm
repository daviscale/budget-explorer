module BudgetExplorer exposing (main)

import Array exposing (Array)
import Browser
import Chart exposing (hBar, title, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias BudgetItem =
    { name : String
    , amount : Float
    }


budgetItemToTuple : BudgetItem -> ( Float, String )
budgetItemToTuple budgetItem =
    ( budgetItem.amount, budgetItem.name )


type alias Model =
    { incomeItems : Array BudgetItem
    , expenseItems : Array BudgetItem
    }


initialModel : Model
initialModel =
    { expenseItems =
        Array.fromList
            [ { name = "Mortgage", amount = 2000.0 }
            , { name = "Groceries", amount = 500.0 }
            , { name = "Utilities", amount = 250.0 }
            ]
    , incomeItems =
        Array.fromList
            [ { name = "Paycheck", amount = 3500.0 } ]
    }


type Msg
    = ItemUpdated BudgetItem


incomeTitle : String
incomeTitle =
    "Income Budget Items"


expenseTitle : String
expenseTitle =
    "Expense Budget Items"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Budget Explorer" ]
        , hBar (List.map budgetItemToTuple (Array.toList model.incomeItems))
            |> Chart.title incomeTitle
            |> toHtml
        , hBar (List.map budgetItemToTuple (Array.toList model.expenseItems))
            |> Chart.title expenseTitle
            |> toHtml
        , htmlForBudgetItems model.incomeItems incomeTitle
        , htmlForBudgetItems model.expenseItems expenseTitle
        ]


htmlForBudgetItems : Array BudgetItem -> String -> Html Msg
htmlForBudgetItems budgetItems title =
    div [] <|
        [ h2 [] [ text title ] ]
            ++ List.map htmlForItem (Array.toIndexedList budgetItems)


htmlForItem : ( Int, BudgetItem ) -> Html Msg
htmlForItem ( index, budgetItem ) =
    let
        indexStr =
            String.fromInt index

        nameId =
            "item-name-" ++ indexStr

        amountId =
            "item-amount-" ++ indexStr
    in
    div [ class "budget-item-entry" ]
        [ label [ for nameId ] [ text "Name: " ]
        , input [ value budgetItem.name, id nameId ] []
        , label [ for amountId ] [ text "Amount: " ]
        , input [ value <| String.fromFloat budgetItem.amount, id amountId ] []
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
