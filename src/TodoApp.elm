module TodoApp exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)

-- this is my todo app :)

type alias Todo =
  { id : Int
  , title : String
  , completed : Bool }

type Filter = All | Active | Completed

type alias Model =
  { todos : List Todo
  , input : String
  , filter : Filter
  , nextId : Int
  , loading : Bool }

-- messages for the app
type Msg = GotTodos (Result Http.Error (List Todo))
  | InputChanged String
  | AddTodo
  | ToggleTodo Int
  | DeleteTodo Int
  | SetFilter Filter
  | ClearCompleted
  | RetryFetch

init : () -> ( Model, Cmd Msg )
init _ =
  ( { todos = []
    , input = ""
    , filter = All
    , nextId = 1000
    , loading = True }
  , fetchTodos )

-- fetch from internet
fetchTodos : Cmd Msg
fetchTodos =
  Http.get
    { url = "https://jsonplaceholder.typicode.com/todos?_limit=10"
    , expect = Http.expectJson GotTodos (Decode.list todoDecoder) }

todoDecoder : Decoder Todo
todoDecoder =
  Decode.map3 Todo
    (Decode.field "id" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "completed" Decode.bool)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotTodos result ->
      case result of
        Ok todos ->
          ( { model | todos = todos, loading = False }, Cmd.none )
        Err _ ->
          ( { model | loading = False }, Cmd.none )

    InputChanged val ->
      ( { model | input = val }, Cmd.none )

    AddTodo ->
      let
        trimmed = String.trim model.input
      in
      if String.isEmpty trimmed then
        ( model, Cmd.none )
      else
        ( { model | todos = model.todos ++ [ { id = model.nextId, title = trimmed, completed = False } ], input = "", nextId = model.nextId + 1 }, Cmd.none )

    ToggleTodo id ->
      ( { model | todos = List.map (\t -> if t.id == id then { t | completed = not t.completed } else t) model.todos }, Cmd.none )

    DeleteTodo id ->
      ( { model | todos = List.filter (\t -> t.id /= id) model.todos }, Cmd.none )

    SetFilter f ->
      ( { model | filter = f }, Cmd.none )

    ClearCompleted ->
      ( { model | todos = List.filter (\t -> not t.completed) model.todos }, Cmd.none )

    RetryFetch ->
      ( { model | loading = True }, fetchTodos )

-- get todos to show based on filter
visibleTodos : Model -> List Todo
visibleTodos model =
  case model.filter of
    All -> model.todos
    Active -> List.filter (\t -> not t.completed) model.todos
    Completed -> List.filter .completed model.todos

view : Model -> Html Msg
view model =
  let
    remaining = List.length (List.filter (\t -> not t.completed) model.todos)
    doneCount = List.length (List.filter .completed model.todos)
  in
  div [ style "font-family" "'Segoe UI', system-ui, sans-serif", style "max-width" "580px", style "margin" "40px auto", style "padding" "0 16px" ]
    [ viewHeader
    , viewInputRow model.input
    , div [ style "background" "white", style "border-radius" "8px", style "box-shadow" "0 2px 20px rgba(0,0,0,0.12)", style "overflow" "hidden" ]
      [ if model.loading then
          viewLoading
        else if List.isEmpty model.todos then
          viewEmpty
        else
          div []
            [ viewFilterBar model.filter remaining
            , ul [ style "margin" "0", style "padding" "0", style "list-style" "none" ]
                (List.map viewTodoItem (visibleTodos model))
            , viewFooter remaining doneCount ] ] ]

viewHeader : Html Msg
viewHeader =
  div [ style "text-align" "center", style "margin-bottom" "24px" ]
    [ h1 [ style "font-size" "3.5rem", style "font-weight" "100", style "color" "#e74c3c", style "letter-spacing" "0.15em", style "margin" "0 0 6px" ]
        [ text "todos" ] ]

viewInputRow : String -> Html Msg
viewInputRow val =
  div [ style "display" "flex", style "gap" "8px", style "margin-bottom" "12px" ]
    [ input [ type_ "text", placeholder "What needs to be done? (Enter to add)", value val, onInput InputChanged, style "flex" "1", style "padding" "14px 16px", style "font-size" "1rem", style "border" "2px solid #e0e0e0", style "border-radius" "6px", style "outline" "none", style "font-family" "inherit" ] []
    , button [ onClick AddTodo, style "padding" "14px 22px", style "background" "#e74c3c", style "color" "white", style "border" "none", style "border-radius" "6px", style "cursor" "pointer", style "font-size" "1rem", style "font-weight" "600" ] [ text "Add" ] ]

viewFilterBar : Filter -> Int -> Html Msg
viewFilterBar current remaining =
  div [ style "display" "flex", style "align-items" "center", style "padding" "10px 16px", style "border-bottom" "1px solid #f0f0f0", style "background" "#fafafa", style "gap" "6px", style "flex-wrap" "wrap" ]
    [ span [ style "font-size" "0.82em", style "color" "#aaa", style "margin-right" "6px" ] [ text (String.fromInt remaining ++ " remaining") ]
    , filterChip "All" All current
    , filterChip "Active" Active current
    , filterChip "Completed" Completed current ]

filterChip : String -> Filter -> Filter -> Html Msg
filterChip label f current =
  let
    active = f == current
  in
  button [ onClick (SetFilter f), style "padding" "3px 12px", style "border" ("1px solid " ++ (if active then "#e74c3c" else "#ddd")), style "border-radius" "12px", style "background" (if active then "#e74c3c" else "transparent"), style "color" (if active then "white" else "#777"), style "cursor" "pointer", style "font-size" "0.82em", style "font-family" "inherit" ]
    [ text label ]

viewTodoItem : Todo -> Html Msg
viewTodoItem todo =
  li [ style "display" "flex", style "align-items" "center", style "padding" "13px 16px", style "border-bottom" "1px solid #f5f5f5", style "gap" "12px" ]
    [ input [ type_ "checkbox", checked todo.completed, onClick (ToggleTodo todo.id), style "width" "18px", style "height" "18px", style "cursor" "pointer", style "accent-color" "#27ae60", style "flex-shrink" "0" ] []
    , span [ style "flex" "1", style "font-size" "1rem", style "text-decoration" (if todo.completed then "line-through" else "none"), style "color" (if todo.completed then "#ccc" else "#333"), style "word-break" "break-word" ] [ text todo.title ]
    , button [ onClick (DeleteTodo todo.id), style "background" "none", style "border" "none", style "cursor" "pointer", style "color" "#ddd", style "font-size" "1.5rem", style "line-height" "1", style "padding" "0 4px", style "flex-shrink" "0" ] [ text "×" ] ]

viewFooter : Int -> Int -> Html Msg
viewFooter remaining doneCount =
  div [ style "display" "flex", style "justify-content" "space-between", style "align-items" "center", style "padding" "10px 16px", style "font-size" "0.82em", style "color" "#aaa", style "border-top" "1px solid #f0f0f0", style "background" "#fafafa" ]
    [ span [] [ text (String.fromInt remaining ++ " item(s) left") ]
    , if doneCount > 0 then
        button [ onClick ClearCompleted, style "background" "none", style "border" "none", style "cursor" "pointer", style "color" "#bbb", style "font-family" "inherit", style "font-size" "0.95em" ] [ text ("Clear completed (" ++ String.fromInt doneCount ++ ")") ]
      else
        text "" ]

viewLoading : Html Msg
viewLoading =
  div [ style "padding" "52px 24px", style "text-align" "center", style "color" "#aaa" ]
    [ p [ style "font-size" "2.5rem", style "margin" "0 0 10px" ] [ text "⏳" ]
    , p [ style "margin" "0", style "font-size" "0.95em" ] [ text "Fetching todos from JSONPlaceholder…" ] ]

viewEmpty : Html Msg
viewEmpty =
  div [ style "padding" "52px 24px", style "text-align" "center", style "color" "#ccc" ]
    [ p [ style "font-size" "2.5rem", style "margin" "0 0 10px" ] [ text "✅" ]
    , p [ style "margin" "0", style "font-size" "0.95em" ] [ text "Nothing here — add a todo above!" ] ]

-- main function runs the app
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none }
