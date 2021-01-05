module BudgetExplorer exposing (main)

import Browser
import Chart exposing (hBar, title, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias BudgetItem =
    { id : Int
    , name : String
    , amount : Float
    }


budgetItemToTuple : BudgetItem -> ( Float, String )
budgetItemToTuple budgetItem =
    ( budgetItem.amount, budgetItem.name )


type alias Model =
    { incomeItems : List BudgetItem
    , expenseItems : List BudgetItem
    }


initialModel : Model
initialModel =
    { expenseItems =
        [ { id = 1, name = "Mortgage", amount = 2000.0 }
        , { id = 2, name = "Groceries", amount = 500.0 }
        , { id = 3, name = "Utilities", amount = 250.0 }
        ]
    , incomeItems =
        [ { id = 1, name = "Paycheck", amount = 3500.0 } ]
    }


type BudgetCategory
    = Income
    | Expense


type Msg
    = NameChanged BudgetCategory String BudgetItem
    | AmountChanged BudgetCategory Float BudgetItem
    | NewItem BudgetCategory
    | RemoveItem BudgetCategory BudgetItem


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
        , hBar (List.map budgetItemToTuple model.incomeItems)
            |> Chart.title incomeTitle
            |> toHtml
        , hBar (List.map budgetItemToTuple model.expenseItems)
            |> Chart.title expenseTitle
            |> toHtml
        , htmlForSummary model
        , htmlForBudgetItems Income model.incomeItems incomeTitle
        , htmlForBudgetItems Expense model.expenseItems expenseTitle
        ]


htmlForSummary : Model -> Html Msg
htmlForSummary model =
    let
        incomeTotal =
            List.foldl (+) 0.0 (List.map (\i -> i.amount) model.incomeItems)

        incomeTotalStr =
            String.fromFloat incomeTotal

        expenseTotal =
            List.foldl (+) 0.0 (List.map (\i -> i.amount) model.expenseItems)

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
            ++ List.map (htmlForItem budgetCategory) budgetItems
            ++ [ button
                    [ type_ "button"
                    , onClick (NewItem budgetCategory)
                    ]
                    [ text ("Add " ++ buttonName budgetCategory) ]
               ]


htmlForItem : BudgetCategory -> BudgetItem -> Html Msg
htmlForItem budgetCategory budgetItem =
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
                , onInput (\newName -> NameChanged budgetCategory newName budgetItem)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ label [ for amountId ] [ text "Amount: " ]
            , input
                [ value <| String.fromFloat budgetItem.amount
                , id amountId
                , onInput (\newAmount -> AmountChanged budgetCategory (Maybe.withDefault 0.0 (String.toFloat newAmount)) budgetItem)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ button
                [ type_ "button"
                , onClick (RemoveItem budgetCategory budgetItem)
                ]
                [ text ("Remove " ++ buttonName budgetCategory) ]
            ]
        ]


replaceBudgetItem : BudgetItem -> BudgetItem -> BudgetItem
replaceBudgetItem budgetItemFromList budgetItemToReplace =
    if budgetItemFromList.id == budgetItemToReplace.id then
        budgetItemToReplace

    else
        budgetItemFromList


updateModelArray : Model -> BudgetItem -> BudgetCategory -> Model
updateModelArray model budgetItem budgetCategory =
    case budgetCategory of
        Income ->
            { model | incomeItems = List.map (\item -> replaceBudgetItem item budgetItem) model.incomeItems }

        Expense ->
            { model | expenseItems = List.map (\item -> replaceBudgetItem item budgetItem) model.expenseItems }


updateName : Model -> BudgetItem -> BudgetCategory -> String -> ( Model, Cmd Msg )
updateName model budgetItem budgetCategory name =
    let
        newItem =
            { budgetItem | name = name }

        newModel =
            updateModelArray model newItem budgetCategory
    in
    ( newModel, Cmd.none )


updateAmount : Model -> BudgetItem -> BudgetCategory -> Float -> ( Model, Cmd Msg )
updateAmount model budgetItem budgetCategory amount =
    let
        newItem =
            { budgetItem | amount = amount }

        newModel =
            updateModelArray model newItem budgetCategory
    in
    ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged budgetCategory name budgetItem ->
            updateName model budgetItem budgetCategory name

        AmountChanged budgetCategory amount budgetItem ->
            updateAmount model budgetItem budgetCategory amount

        NewItem Income ->
            ( { model | incomeItems = List.append model.incomeItems [ { id = List.length model.incomeItems + 1, name = "", amount = 0.0 } ] }, Cmd.none )

        NewItem Expense ->
            ( { model | expenseItems = List.append model.expenseItems [ { id = List.length model.expenseItems + 1, name = "", amount = 0.0 } ] }, Cmd.none )

        RemoveItem Income budgetItem ->
            ( { model | incomeItems = removeBudgetItemAndResetIds model.incomeItems budgetItem }, Cmd.none )

        RemoveItem Expense budgetItem ->
            ( { model | expenseItems = removeBudgetItemAndResetIds model.expenseItems budgetItem }, Cmd.none )


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
