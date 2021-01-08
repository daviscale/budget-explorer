module BudgetExplorer exposing (main)

import Browser
import Chart exposing (hBar, title, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type BudgetCategory
    = Income
    | Expense


type alias BudgetItem =
    { id : Int
    , name : String
    , amount : Float
    , budgetCategory : BudgetCategory
    }


budgetItemToTuple : BudgetItem -> ( Float, String )
budgetItemToTuple budgetItem =
    ( budgetItem.amount, budgetItem.name )


type alias Model =
    { budgetItems : List BudgetItem
    }


filterByCategory : List BudgetItem -> BudgetCategory -> List BudgetItem
filterByCategory budgetItems budgetCategory =
    List.filter (\budgetItem -> budgetItem.budgetCategory == budgetCategory) budgetItems


incomeItems : List BudgetItem -> List BudgetItem
incomeItems budgetItems =
    filterByCategory budgetItems Income


expenseItems : List BudgetItem -> List BudgetItem
expenseItems budgetItems =
    filterByCategory budgetItems Expense


initialModel : Model
initialModel =
    { budgetItems =
        [ { id = 1, name = "Mortgage", amount = 2000.0, budgetCategory = Expense }
        , { id = 2, name = "Groceries", amount = 500.0, budgetCategory = Expense }
        , { id = 3, name = "Utilities", amount = 250.0, budgetCategory = Expense }
        , { id = 4, name = "Paycheck", amount = 3500.0, budgetCategory = Income }
        ]
    }


type Msg
    = NameChanged String BudgetItem
    | AmountChanged Float BudgetItem
    | NewItem BudgetCategory
    | RemoveItem BudgetItem


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
        , hBar (List.map budgetItemToTuple (incomeItems model.budgetItems))
            |> Chart.title incomeTitle
            |> toHtml
        , hBar (List.map budgetItemToTuple (expenseItems model.budgetItems))
            |> Chart.title expenseTitle
            |> toHtml
        , htmlForSummary model
        , htmlForBudgetItems Income (incomeItems model.budgetItems) incomeTitle
        , htmlForBudgetItems Expense (expenseItems model.budgetItems) expenseTitle
        ]


htmlForSummary : Model -> Html Msg
htmlForSummary model =
    let
        incomeTotal =
            List.foldl (+) 0.0 (List.map (\i -> i.amount) (incomeItems model.budgetItems))

        incomeTotalStr =
            String.fromFloat incomeTotal

        expenseTotal =
            List.foldl (+) 0.0 (List.map (\i -> i.amount) (expenseItems model.budgetItems))

        expenseTotalStr =
            String.fromFloat expenseTotal

        cashLeftOverStr =
            String.fromFloat (incomeTotal - expenseTotal)
    in
    div [ id "summary-section" ]
        [ h2 [] [ text "Summary" ]
        , ul
            []
            [ li [] [ text <| "Income Total: " ++ incomeTotalStr ]
            , li [] [ text <| "Expense Total: " ++ expenseTotalStr ]
            , li [] [ text <| "Cash Left Over: " ++ cashLeftOverStr ]
            ]
        ]


buttonName : BudgetCategory -> String
buttonName budgetCategory =
    case budgetCategory of
        Expense ->
            "Expense Item"

        Income ->
            "Income Item"


htmlForBudgetItems : BudgetCategory -> List BudgetItem -> String -> Html Msg
htmlForBudgetItems budgetCategory budgetItems title =
    div [ class "budget-item-container" ] <|
        [ h2 [] [ text title ] ]
            ++ List.map htmlForItem budgetItems
            ++ [ button
                    [ type_ "button"
                    , onClick (NewItem budgetCategory)
                    ]
                    [ text ("Add " ++ buttonName budgetCategory) ]
               ]


htmlForItem : BudgetItem -> Html Msg
htmlForItem budgetItem =
    let
        idStr =
            String.fromInt budgetItem.id

        nameId =
            "item-name-" ++ idStr

        amountId =
            "item-amount-" ++ idStr

        buttonId =
            "item-button-" ++ idStr
    in
    div [ class "budget-item-row" ]
        [ span [ class "budget-item-field" ]
            [ label [ for nameId ] [ text "Name: " ]
            , input
                [ value budgetItem.name
                , id nameId
                , onInput (\newName -> NameChanged newName budgetItem)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ label [ for amountId ] [ text "Amount: " ]
            , input
                [ value <| String.fromFloat budgetItem.amount
                , id amountId
                , onInput (\newAmount -> AmountChanged (Maybe.withDefault 0.0 (String.toFloat newAmount)) budgetItem)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ button
                [ type_ "button"
                , onClick (RemoveItem budgetItem)
                ]
                [ text ("Remove " ++ buttonName budgetItem.budgetCategory) ]
            ]
        ]


replaceBudgetItem : BudgetItem -> BudgetItem -> BudgetItem
replaceBudgetItem budgetItemFromList budgetItemToReplace =
    if budgetItemFromList.id == budgetItemToReplace.id then
        budgetItemToReplace

    else
        budgetItemFromList


updateModelArray : Model -> BudgetItem -> Model
updateModelArray model budgetItem =
    { model | budgetItems = List.map (\item -> replaceBudgetItem item budgetItem) model.budgetItems }


updateName : Model -> BudgetItem -> String -> ( Model, Cmd Msg )
updateName model budgetItem name =
    let
        newItem =
            { budgetItem | name = name }

        newModel =
            updateModelArray model newItem
    in
    ( newModel, Cmd.none )


updateAmount : Model -> BudgetItem -> Float -> ( Model, Cmd Msg )
updateAmount model budgetItem amount =
    let
        newItem =
            { budgetItem | amount = amount }

        newModel =
            updateModelArray model newItem
    in
    ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged name budgetItem ->
            updateName model budgetItem name

        AmountChanged amount budgetItem ->
            updateAmount model budgetItem amount

        NewItem budgetCategory ->
            ( { model | budgetItems = List.append model.budgetItems [ { id = List.length model.budgetItems + 1, name = "", amount = 0.0, budgetCategory = budgetCategory } ] }, Cmd.none )

        RemoveItem budgetItem ->
            ( { model | budgetItems = removeBudgetItemAndResetIds model.budgetItems budgetItem }, Cmd.none )


removeBudgetItemAndResetIds : List BudgetItem -> BudgetItem -> List BudgetItem
removeBudgetItemAndResetIds budgetItems budgetItem =
    List.filter (\item -> budgetItem.id /= item.id) budgetItems
        |> List.indexedMap (\index item -> { item | id = index + 1 })


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
