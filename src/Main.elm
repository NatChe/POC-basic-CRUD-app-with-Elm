module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Attributes exposing (style)


-- MAIN
main = 
  Browser.sandbox { init = init, update = update, view = view >> toUnstyled }

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
    , amount = ""
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
                        , amount = ""
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
                        , amount = ""
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

-- STYLED
container : List (Attribute msg) -> List (Html msg) -> Html msg
container = 
    styled div
        [ fontFamilies [ "Helvetica", "sans-serif" ] 
        , Css.width (vw 100)
        , Css.height (vh 100)
        , textAlign center
        , backgroundColor (hex "#f2f2f2")
        , padding (px 20)
        ]

blockStyle : Style
blockStyle = 
    Css.batch
        [ Css.width (pct 60)
        , margin2 (px 16) (auto)
        , border3 (px 1) solid (hex "#ddd")
        , padding (px 16)
        , boxSizing borderBox
        , backgroundColor (hex "#fff")
        ]

cellStyle: Style
cellStyle =
    Css.batch
        [ border3 (px 1) solid (hex "#ddd")
        , textAlign left
        , fontWeight normal
        , padding (px 8)
        ]

buttonStyle: Style
buttonStyle = 
    Css.batch
        [ padding2 (px 8) (px 16)
        , borderRadius (px 2)
        , cursor pointer
        ]
      
styledTable : List (Attribute msg) -> List (Html msg) -> Html msg
styledTable = 
    styled Html.Styled.table
        [ blockStyle 
        , borderCollapse collapse
        ]

headCell: List (Attribute msg) -> List (Html msg) -> Html msg
headCell = 
    styled th
        [ cellStyle
        , textTransform uppercase
        ]

bodyCell: List (Attribute msg) -> List (Html msg) -> Html msg
bodyCell = 
    styled th
        [ cellStyle ]

totalCell: List (Attribute msg) -> List (Html msg) -> Html msg
totalCell = 
    styled td
        [ textAlign right
        , border3 (px 1) solid (hex "#ddd") 
        , fontWeight bold
        , padding (px 8)
        ]

styledForm : List (Attribute msg) -> List (Html msg) -> Html msg
styledForm = 
    styled Html.Styled.form
        [ blockStyle
        , textAlign center
        ]

primaryButton: List (Attribute msg) -> List (Html msg) -> Html msg
primaryButton = 
    styled button
        [ buttonStyle
        , backgroundColor (hex "#11bf96")
        , color (hex "#fff")
        , border (px 0)
        ]

secondaryButton: List (Attribute msg) -> List (Html msg) -> Html msg
secondaryButton = 
    styled button
        [ buttonStyle
        , backgroundColor (hex "#fff")
        , color (hex "#11bf96")
        , border3 (px 1) solid (hex "#11bf96")
        , marginLeft (px 8)
        ]

styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled Html.Styled.input
        [ display inlineBlock
        , padding2 (px 8) (px 16)
        , margin (px 8)
        , border3 (px 1) solid (hex "#ddd") 
        , borderRadius (px 2)
        ]

-- COMPONENTS

viewForm : Model -> Html Msg
viewForm model = 
    if model.showForm then
        styledForm [onSubmit SaveExpense]
            [ styledInput 
                [ type_ "text"
                , placeholder "Put the description here"
                , value model.description
                , onInput DescriptionInput
                ]
                []
            , styledInput 
                [ type_ "number"
                , placeholder "Put the amount here"
                , value model.amount
                , onInput AmountInput
                ]
                []
            , div []
                [ primaryButton [] [ text "Submit"] ]    
            ]
    else
        div []
            [ primaryButton [ onClick ShowForm ] [ text "Add an expense" ] ]


viewExpenses: Model -> Html Msg
viewExpenses model =
    div []
        [ styledTable []
            [ thead []
                [ tr []
                    [ headCell [] [ text "Expense" ]
                    , headCell [] [ text "Amount" ]
                    , headCell [] [ text ""]
                    ]
                ] 
            , tbody [] 
              ( []
                ++ List.map toTableRow model.expenses
                ++ [
                    tr []
                        [ totalCell [] [ text "Total"]
                        , totalCell [] [ text (model.expenses |> calcTotal |> String.fromFloat)]
                        , totalCell [] []
                        ]
                ]
              )
            ]
        ]


toTableRow : Expense -> Html Msg
toTableRow expense =
    tr []
        [ bodyCell [] [ text expense.description ]
        , bodyCell [] [ text (String.fromFloat (Maybe.withDefault 0 expense.amount)) ]
        , bodyCell [] 
            [ secondaryButton [ onClick (DeleteExpense expense.id) ] [ text "Delete"] 
            , secondaryButton [ onClick (EditExpense expense) ] [ text "Edit"] 
            ]
        ]


calcTotal : List Expense -> Float
calcTotal expenses =
    expenses
      |> List.map (\n -> Maybe.withDefault 0 n.amount)
      |> List.foldl (+) 0

view : Model -> Html Msg
view model =
    container []
        [ viewExpenses model
        , viewForm model
        ]