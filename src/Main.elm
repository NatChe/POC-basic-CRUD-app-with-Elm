module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MAIN
main = 
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
    { description: String
    , amount: String
    , showForm: Bool
    , nextId: Int
    , editId: Maybe Int
    , expenses: List Expense
    }


type alias Expense = 
    { id: Int
    , description: String
    , amount: Maybe Float
    }

init: Model
init = 
    { description = ""
    , amount = "0"
    , showForm = False
    , nextId = 2
    , editId = Nothing
    , expenses = [{ id = 1, description = "Food", amount = Just 25.0 }]
    }


type Msg
    = ShowForm
    | DescriptionInput String
    | AmountInput String
    | SaveExpense
    | DeleteExpense Int
    | EditExpense Expense


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowForm ->
            { model | showForm = not model.showForm }

        DescriptionInput description ->
            { model | description = description }

        AmountInput amount ->
            { model | amount = amount }

        SaveExpense ->
            case model.editId of
                -- Add new expense
                Nothing ->
                    { model 
                        | expenses = model.expenses ++ [{ id = model.nextId, description = model.description, amount = String.toFloat(model.amount) }]
                        , description = ""
                        , amount = "0"
                        , nextId = model.nextId + 1
                        , showForm = False
                    }

                -- Modify existing expense
                Just editId ->
                    { model 
                        | expenses = model.expenses 
                            |> List.map (\expense ->
                                if expense.id == editId then
                                    { expense | description = model.description, amount = String.toFloat(model.amount)}
                                else
                                    expense
                                )
                        , description = ""
                        , amount = "0"
                        , editId = Nothing
                        , showForm = False
                    }

        DeleteExpense id ->
            { model | expenses = List.filter (\expense -> expense.id /= id) model.expenses }

        EditExpense expense ->
            { model
                | editId = Just expense.id
                , description = expense.description
                , amount = String.fromFloat (Maybe.withDefault 0 expense.amount)
                , showForm = True
            }  
        
-- VIEW
viewForm : Model -> Html Msg
viewForm model = 
    if model.showForm then
        Html.form [onSubmit SaveExpense]
            [ div []
                [ input 
                    [ type_ "text"
                    , placeholder "Put the description here"
                    , value model.description
                    , onInput DescriptionInput
                    ]
                    []
                ]
            , div []
                [ input 
                    [ type_ "number"
                    , placeholder "Put the amount here"
                    , value model.amount
                    , onInput AmountInput
                    ]
                    []
                ]
            , div []
                [ button [] [ text "Submit"] ]    
            ]
    else
        div []
            [ button [ onClick ShowForm ] [ text "Add an expense" ] ]


viewExpenses: Model -> Html Msg
viewExpenses model =
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Expense" ]
                    , th [] [ text "Amount" ]
                    , th [] [ text ""]
                    ]
                ] 
            , tbody [] 
              ( []
                ++ List.map toTableRow model.expenses
                ++ [
                    tr []
                        [ th [] [ text "Total"]
                        , th [] [ text (model.expenses |> calcTotal |> String.fromFloat)]
                        ]
                ]
              )
            ]
        ]


toTableRow : Expense -> Html Msg
toTableRow expense =
    tr []
        [ td [] [ text expense.description ]
        , td [] [ text (String.fromFloat (Maybe.withDefault 0 expense.amount)) ]
        , td [] 
            [ button [ onClick (DeleteExpense expense.id) ] [ text "-"] 
            , button [ onClick (EditExpense expense) ] [ text "Edit"] 
            ]
        ]


calcTotal : List Expense -> Float
calcTotal expenses =
    expenses
      |> List.map (\n -> Maybe.withDefault 0 n.amount)
      |> List.foldl (+) 0

view : Model -> Html Msg
view model =
    div []
        [ viewForm model
        , viewExpenses model
        ]