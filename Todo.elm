port module Todo exposing (..)
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Json.Decode exposing ((:=), andThen)
import String
import Json.Encode
import List
import Debug


main : Program (Maybe Json.Encode.Value)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = (\msg model -> withSetStorage (update msg model))
    , subscriptions = \_ -> Sub.none
    }


port setStorage : Json.Encode.Value -> Cmd msg

port focus : String -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage (model, cmds) =
  ( model, Cmd.batch [ setStorage (modelToValue model), cmds ] )



-- MODEL

-- The full application state of our todo app.
type alias Model =
    { tasks : List Task
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Task =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , importance : Importance
    }

type Importance
  = Normal
  | High
  | Low

emptyModel : Model
emptyModel =
  { tasks = []
  , visibility = "All"
  , field = ""
  , uid = 0
  }


newTask : String -> Int -> Task
newTask desc id =
  { description = desc
  , completed = False
  , editing = False
  , id = id
  , importance = Normal
  }


init : Maybe Json.Encode.Value -> ( Model, Cmd Msg )
init savedModel =
  case savedModel of
    Just value -> Maybe.withDefault emptyModel (Json.decodeValue modelDecoder value |> resultToMaybe) ! []
    _ -> emptyModel ! []

-- Json.Decode

modelDecoder : Json.Decoder Model
modelDecoder =
  Json.object4 Model
    ("tasks" := Json.list taskDecoder)
    ("field" := Json.string)
    ("uid" := Json.int)
    ("visibility" := Json.string)

taskDecoder : Json.Decoder Task
taskDecoder =
  Json.object5 Task
    ("description" := Json.string)
    ("completed" := Json.bool)
    ("editing" := Json.bool)
    ("id" := Json.int)
    ("importance" := Json.string `andThen` importanceDecoder)

importanceDecoder : String -> Json.Decoder Importance
importanceDecoder tag =
  case tag of
    "Normal" -> Json.succeed Normal
    "High" -> Json.succeed High
    "Low" -> Json.succeed Low
    _ -> Json.fail (tag ++ " is not a recognized tag for Importance")

resultToMaybe : Result String Model -> Maybe Model
resultToMaybe result =
  case result of
    Result.Ok model -> Just model
    Result.Err error -> Debug.log error Nothing

-- Json.Encode

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
    [
      ("tasks", Json.Encode.list (List.map taskToValue model.tasks)),
      ("field", Json.Encode.string model.field),
      ("uid", Json.Encode.int model.uid),
      ("visibility", Json.Encode.string model.visibility)
    ]

taskToValue : Task -> Json.Encode.Value
taskToValue task =
  Json.Encode.object
    [
      ("description", Json.Encode.string task.description),
      ("completed", Json.Encode.bool task.completed),
      ("editing", Json.Encode.bool task.editing),
      ("id", Json.Encode.int task.id),
      ("importance", importanceToValue task.importance)
    ]

importanceToValue : Importance -> Json.Encode.Value
importanceToValue importance =
  case importance of
    Normal -> Json.Encode.string "Normal"
    High -> Json.Encode.string "High"
    Low -> Json.Encode.string "Low"

-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Add ->
      { model
        | uid = model.uid + 1
        , field = ""
        , tasks =
            if String.isEmpty model.field then
              model.tasks
            else
              model.tasks ++ [newTask model.field model.uid]
      }
        ! []

    UpdateField str ->
      { model | field = str }
        ! []

    EditingTask id isEditing ->
      let
        updateTask t =
          if t.id == id then { t | editing = isEditing } else t
      in
        { model | tasks = List.map updateTask model.tasks }
          ! [ focus ("#todo-" ++ toString id) ]

    UpdateTask id task ->
      let
        updateTask t =
          if t.id == id then { t | description = task } else t
      in
        { model | tasks = List.map updateTask model.tasks }
          ! []

    Delete id ->
      { model | tasks = List.filter (\t -> t.id /= id) model.tasks }
        ! []

    DeleteComplete ->
      { model | tasks = List.filter (not << .completed) model.tasks }
        ! []

    Check id isCompleted ->
      let
        updateTask t =
          if t.id == id then { t | completed = isCompleted } else t
      in
        { model | tasks = List.map updateTask model.tasks }
          ! []

    CheckAll isCompleted ->
      let
        updateTask t =
          { t | completed = isCompleted }
      in
        { model | tasks = List.map updateTask model.tasks }
          ! []

    ChangeVisibility visibility ->
      { model | visibility = visibility }
        ! []



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper"
    , style [ ("visibility", "hidden") ]
    ]
    [ section
        [ id "todoapp" ]
        [ lazy taskEntry model.field
        , lazy2 taskList model.visibility model.tasks
        , lazy2 controls model.visibility model.tasks
        ]
    , infoFooter
    ]


onEnter : msg -> msg -> Attribute msg
onEnter fail success =
  let
    tagger code =
      if code == 13 then success
      else fail
  in
    on "keyup" (Json.map tagger keyCode)


taskEntry : String -> Html Msg
taskEntry task =
  header
    [ id "header" ]
    [ h1 [] [ text "todos" ]
    , input
        [ id "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , value task
        , name "newTodo"
        , on "input" (Json.map UpdateField targetValue)
        , onEnter NoOp Add
        ]
        []
    ]


taskList : String -> List Task -> Html Msg
taskList visibility tasks =
  let
    isVisible todo =
      case visibility of
        "Completed" -> todo.completed
        "Active" -> not todo.completed
        _ -> True

    allCompleted =
      List.all .completed tasks

    cssVisibility =
      if List.isEmpty tasks then "hidden" else "visible"
  in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick (CheckAll (not allCompleted))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map (todoItem) (List.filter isVisible tasks))
      ]


todoItem : Task -> Html Msg
todoItem todo =
  li
    [ classList [ ("completed", todo.completed), ("editing", todo.editing) ] ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type' "checkbox"
            , checked todo.completed
            , onClick (Check todo.id (not todo.completed))
            ]
            []
        , label
            [ onDoubleClick (EditingTask todo.id True) ]
            [ text todo.description ]
        , button
            [ class "destroy"
            , onClick (Delete todo.id)
            ]
            []
        ]
    , input
        [ class "edit"
        , value todo.description
        , name "title"
        , id ("todo-" ++ toString todo.id)
        , on "input" (Json.map (UpdateTask todo.id) targetValue)
        , onBlur (EditingTask todo.id False)
        , onEnter NoOp (EditingTask todo.id False)
        ]
        []
    ]


controls : String -> List Task -> Html Msg
controls visibility tasks =
  let
    tasksCompleted =
      List.length (List.filter .completed tasks)

    tasksLeft =
      List.length tasks - tasksCompleted

    item_ =
      if tasksLeft == 1 then " item" else " items"
  in
    footer
      [ id "footer"
      , hidden (List.isEmpty tasks)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ visibilitySwap "#/" "All" visibility
          , text " "
          , visibilitySwap "#/active" "Active" visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onClick DeleteComplete
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri, classList [("selected", visibility == actualVisibility)] ]
        [ text visibility ]
    ]


infoFooter : Html msg
infoFooter =
  footer [ id "info" ]
    [ p [] [ text "Double-click to edit a todo" ]
    , p []
        [ text "Written by "
        , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
        ]
    , p []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]
