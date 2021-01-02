module BudgetExplorer exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Chart exposing (hBar, title, toHtml)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


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


type BudgetCategory
    = Income
    | Expense


type Msg
    = NameChanged BudgetCategory String Int
    | AmountChanged BudgetCategory Float Int
    | NewItem BudgetCategory
    | RemoveItem BudgetCategory Int


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
        , htmlForSummary model
        , htmlForBudgetItems Income model.incomeItems incomeTitle
        , htmlForBudgetItems Expense model.expenseItems expenseTitle
        ]


htmlForSummary : Model -> Html Msg
htmlForSummary model =
    let
        incomeTotal =
            Array.foldl (+) 0.0 (Array.map (\i -> i.amount) model.incomeItems)

        incomeTotalStr =
            String.fromFloat incomeTotal

        expenseTotal =
            Array.foldl (+) 0.0 (Array.map (\i -> i.amount) model.expenseItems)

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


htmlForBudgetItems : BudgetCategory -> Array BudgetItem -> String -> Html Msg
htmlForBudgetItems budgetCategory budgetItems title =
    div [ class "budget-item-container" ] <|
        [ h2 [] [ text title ] ]
            ++ List.map (htmlForItem budgetCategory) (Array.toIndexedList budgetItems)
            ++ [ button
                    [ type_ "button"
                    , onClick (NewItem budgetCategory)
                    ]
                    [ text ("Add " ++ buttonName budgetCategory) ]
               ]


htmlForItem : BudgetCategory -> ( Int, BudgetItem ) -> Html Msg
htmlForItem budgetCategory ( index, budgetItem ) =
    let
        indexStr =
            String.fromInt index

        nameId =
            "item-name-" ++ indexStr

        amountId =
            "item-amount-" ++ indexStr

        buttonId =
            "item-button-" ++ indexStr
    in
    div [ class "budget-item-row" ]
        [ span [ class "budget-item-field" ]
            [ label [ for nameId ] [ text "Name: " ]
            , input
                [ value budgetItem.name
                , id nameId
                , onInput (\newName -> NameChanged budgetCategory newName index)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ label [ for amountId ] [ text "Amount: " ]
            , input
                [ value <| String.fromFloat budgetItem.amount
                , id amountId
                , onInput (\newAmount -> AmountChanged budgetCategory (Maybe.withDefault 0.0 (String.toFloat newAmount)) index)
                ]
                []
            ]
        , span [ class "budget-item-field" ]
            [ button
                [ type_ "button"
                , onClick (RemoveItem budgetCategory index)
                ]
                [ text ("Remove " ++ buttonName budgetCategory) ]
            ]
        ]


updateModelArray : Model -> BudgetItem -> BudgetCategory -> Int -> Model
updateModelArray model budgetItem budgetCategory index =
    case budgetCategory of
        Income ->
            { model | incomeItems = Array.set index budgetItem model.incomeItems }

        Expense ->
            { model | expenseItems = Array.set index budgetItem model.expenseItems }


updateName : Model -> BudgetItem -> BudgetCategory -> String -> Int -> ( Model, Cmd Msg )
updateName model budgetItem budgetCategory name index =
    let
        newItem =
            { budgetItem | name = name }

        newModel =
            updateModelArray model newItem budgetCategory index
    in
    ( newModel, Cmd.none )


updateAmount : Model -> BudgetItem -> BudgetCategory -> Float -> Int -> ( Model, Cmd Msg )
updateAmount model budgetItem budgetCategory amount index =
    let
        newItem =
            { budgetItem | amount = amount }

        newModel =
            updateModelArray model newItem budgetCategory index
    in
    ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged Income name index ->
            case Array.get index model.incomeItems of
                Just incomeItem ->
                    updateName model incomeItem Income name index

                Nothing ->
                    ( model, Cmd.none )

        NameChanged Expense name index ->
            case Array.get index model.expenseItems of
                Just expenseItem ->
                    updateName model expenseItem Expense name index

                Nothing ->
                    ( model, Cmd.none )

        AmountChanged Income amount index ->
            case Array.get index model.incomeItems of
                Just incomeItem ->
                    updateAmount model incomeItem Income amount index

                Nothing ->
                    ( model, Cmd.none )

        AmountChanged Expense amount index ->
            case Array.get index model.expenseItems of
                Just expenseItem ->
                    updateAmount model expenseItem Expense amount index

                Nothing ->
                    ( model, Cmd.none )

        NewItem Income ->
            ( { model | incomeItems = Array.push { name = "", amount = 0.0 } model.incomeItems }, Cmd.none )

        NewItem Expense ->
            ( { model | expenseItems = Array.push { name = "", amount = 0.0 } model.expenseItems }, Cmd.none )

        RemoveItem Income index ->
            ( { model | incomeItems = Array.removeAt index model.incomeItems }, Cmd.none )

        RemoveItem Expense index ->
            ( { model | expenseItems = Array.removeAt index model.expenseItems }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
