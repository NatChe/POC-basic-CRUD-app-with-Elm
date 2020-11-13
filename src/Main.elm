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
    , amount: Int
    , showForm: Bool
    , nextId: Int
    , editId: Maybe Int
    , expenses: List Expense
    }


type alias Expense = 
    { id: Int
    , description: String
    , amount: Int
    }

init: Model
init = 
    { description = ""
    , amount = 0
    , showForm = False
    , nextId = 0
    , editId = Nothing
    , expenses = []
    }


type Msg
    = ShowForm
    | DescriptionInput String
    | AmountInput Int
   {-} | SaveExpense String Int
    | DeleteExpense
    | EditExpense -}


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowForm ->
            { model | showForm = not model.showForm }

        DescriptionInput description ->
            { model | description = description }

        AmountInput amount ->
            { model | amount = amount }

        
-- VIEW
viewForm : Model -> Html Msg
viewForm model = 
    if model.showForm then
        Html.form []
            [ div []
                [ input 
                    [ type_ "text"
                    , placeholder "Put the description here"
                    , value model.description
                    ]
                    []
                ]
            , div []
                [ input 
                    [ type_ "number"
                    , placeholder "Put the amount here"
                    , value (String.fromInt model.amount)
                    ]
                    []
                ]
            , div []
                [ button [] [ text "Submit"] ]    
            ]
    else
        div []
            [ button [ onClick ShowForm ] [ text "Add an expense" ] ]


view : Model -> Html Msg
view model =
    div []
        [ viewForm model ]