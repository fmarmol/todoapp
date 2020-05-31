module Main exposing (main)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { tasks : List Task
    , current : Task
    }


type alias Task =
    { id : Int
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] { id = 0, title = "" }, Cmd.none )


type Msg
    = Input String
    | Save
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | current = Task 0 value }, Cmd.none )

        Save ->
            if model.current.title == "" then
                ( model, Cmd.none )

            else
                let
                    modelAfterSave =
                        saveInput model model.current.title

                    modelAfterReset =
                        reset modelAfterSave
                in
                ( modelAfterReset, Cmd.none )

        Delete id ->
            ( deleteTask model id, Cmd.none )


reset : Model -> Model
reset model =
    { model | current = Task 0 "" }


saveInput : Model -> String -> Model
saveInput model value =
    let
        id =
            List.length model.tasks + 1

        task =
            Task id value
    in
    { model | tasks = task :: model.tasks }


deleteTask : Model -> Int -> Model
deleteTask model id =
    let
        newTasks =
            List.filter
                (\task -> task.id /= id)
                model.tasks
    in
    { model | tasks = newTasks }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Element.layout []
        (Element.column
            [ Element.centerY, Element.centerX ]
            [ Element.text "tasks"
            , todoListElem model.tasks
            , Element.row [ Element.spaceEvenly, Element.width Element.fill ] [ inputElem model.current.title, buttonElem ]
            ]
        )


inputElem : String -> Element.Element Msg
inputElem value =
    Element.Input.text []
        { onChange = Input
        , placeholder = Just (Element.Input.placeholder [] (Element.text "new task"))
        , label = Element.Input.labelHidden "test"
        , text = value
        }


buttonElem : Element.Element Msg
buttonElem =
    Element.Input.button
        [ Element.Background.color (Element.rgb255 50 119 168)
        , Element.Border.solid
        , Element.Border.rounded 3
        , Element.height Element.fill
        , Element.Border.width 1
        , Element.Border.color (Element.rgb255 50 119 168)
        ]
        { onPress = Just Save
        , label = Element.el [ Element.Font.color (Element.rgb255 255 255 255) ] (Element.text "save")
        }


todoListElem : List Task -> Element.Element Msg
todoListElem tasks =
    Element.column
        [ Element.width Element.fill
        ]
        (List.map todoElem tasks)


todoElem : Task -> Element.Element Msg
todoElem task =
    Element.wrappedRow [ Element.width Element.fill, Element.spaceEvenly ]
        [ Element.text task.title
        , Element.Input.button
            [ Element.Background.color (Element.rgb255 150 48 35)
            , Element.Border.solid
            , Element.Border.width 1
            , Element.Border.rounded 2
            ]
            { onPress = Just (Delete task.id), label = Element.text "delete" }
        ]
