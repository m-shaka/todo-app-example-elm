import Html exposing (..)
import Html.Attributes exposing (style, type_, value, href)
import Html.Events exposing (onClick, onSubmit, onInput, onWithOptions, on, defaultOptions)
import Json.Decode as Json

main = Html.beginnerProgram {
    view = view,
    model = model,
    update = update
  }

-- MODEL
type alias Model = {
  todos: List Todo,
  filter: Filter,
  input_: String
}

model : Model
model = {todos = [], filter = SHOW_ALL, input_ = ""}

-- UPDATE
type Filter =
  SHOW_COMPLETED
  | SHOW_ACTIVE
  | SHOW_ALL

type alias Todo = {task: String, completed: Bool}

type Msg =
  ADD_TODO
  | SET_VISIBILITY_FILTER Filter
  | TOGGLE_TODO Int
  | CHANGE_INPUT String
  | Default

update : Msg -> Model -> Model
update msg model = case msg of
  ADD_TODO ->
    let newTodos = Todo model.input_ False :: model.todos in
    {model | todos = newTodos, input_ = ""}
  SET_VISIBILITY_FILTER filter_ -> {model | filter = filter_}
  TOGGLE_TODO id_ ->
    let toggle index todo =
      if id_ == index then {todo | completed = not todo.completed} else todo
    in
    {model | todos = (List.indexedMap toggle model.todos)}
  CHANGE_INPUT newInput -> {model | input_ = newInput}
  Default -> model

-- VIEW
view : Model -> Html Msg
view model =
  let
    render = Debug.log "render" model
    onSubmit_ =
      let msg =
        if model.input_ == "" then Default else ADD_TODO
      in
      let options =
        {defaultOptions | preventDefault = True}
      in
      onWithOptions "submit" options (Json.succeed msg)
    addTodoForm = form [onSubmit_] [
      input [type_ "text", onInput CHANGE_INPUT, value model.input_] [],
      input [type_ "submit", value "Add Todo"] []
    ]
    todos = case model.filter of
      SHOW_ALL -> model.todos
      SHOW_COMPLETED -> List.filter (\todo -> todo.completed) model.todos
      SHOW_ACTIVE -> List.filter (\todo -> not todo.completed) model.todos
    style_ = [("margin", "8px"), ("display", "block")]
  in
  div [style style_] [
    addTodoForm,
    ul [] (List.indexedMap viewTodo todos),
    footer model.filter
  ]

viewTodo: Int -> Todo -> Html Msg
viewTodo id_ todo =
  let style_ =
    [("textDecoration", if todo.completed then "line-through" else "none")]
  in
  li [onClick (TOGGLE_TODO id_), style style_] [text todo.task]

footer: Filter -> Html Msg
footer current =
  let
    options = {defaultOptions | preventDefault = True}
    onClick_ msg =
      onWithOptions "click" options (Json.succeed msg)
    filterLink filter_ s =
      if current == filter_ then span [] [text s]
      else a [href "", onClick_ (SET_VISIBILITY_FILTER filter_)] [text s]
  in
  p [] [
    text "Show: ",
    filterLink SHOW_ALL "All",
    text ", ",
    filterLink SHOW_ACTIVE "Active",
    text ", ",
    filterLink SHOW_COMPLETED "Completed"
  ]
