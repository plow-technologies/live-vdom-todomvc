{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Lib where

import           Types

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.Sequence          as S


import           Data.JSString          as JS
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import           GHCJS.VDOM.Event
import           LiveVDom
import           LiveVDom.Adapter.Types
import qualified LiveVDom.Types         as T
import           Valentine



-- Please make sure this is removed.
import           Unsafe.Coerce

  -- <head>
  --   <meta charset="utf-8">
  --   <title>GHCJS LiveVDom • TodoMVC</title>
  --   <link rel="stylesheet" href="node_modules/todomvc-common/base.css">
  --   <link rel="stylesheet" href="node_modules/todomvc-app-css/index.css">
  --   <style>[ng-cloak] { display: none; }</style>
  -- </head>

-- | Make a Todo item from a title
mkTodo :: JSString -> Todo
mkTodo title = Todo title False False True

-- | Add a Todo item the the front of a sequence
addTodo :: STMMailbox (S.Seq Todo) -> JSString -> Message ()
addTodo mb title = modifyMailbox mb appendNew
  where appendNew = (mkTodo title S.<|)

-- | Set a Todo item as completed
toggleCompleted :: STMMailbox (S.Seq Todo) -> Int -> Message ()
toggleCompleted mb i = modifyMailbox mb toggleAt
  where toggleAt = over (ix i . todoCompleted) not

-- | Remove all completed Todo items
clearCompleted :: STMMailbox (S.Seq Todo) -> Message ()
clearCompleted mb = modifyMailbox mb removeCompleted
  where removeCompleted = S.filter (not . view todoCompleted)

-- | Remove a single Todo item
removeTodo :: STMMailbox (S.Seq Todo) -> Int -> Message ()
removeTodo mb i = modifyMailbox mb (removeAt i)

-- | Remove an item in a sequence at a location
removeAt :: Int -> S.Seq a -> S.Seq a
removeAt i xs = case S.viewl back of
                  S.EmptyL -> front
                  (_ S.:< back') -> front S.>< back'
  where (front, back) = S.splitAt i xs

-- | Set all items as completed
toggleAll :: STMMailbox (S.Seq Todo) -> Message ()
toggleAll mb = modifyMailbox mb toggle
  where toggle = set (each . todoCompleted) True

-- | Set a Todo item as editable
editTodo :: STMMailbox (S.Seq Todo) -> Int -> Message ()
editTodo mb i = modifyMailbox mb $ over (ix i) setEdit

setEdit :: Todo -> Todo
setEdit = set todoEdit True


-- | Apply the filter to the Todo sequence
filterTodo :: TodoFilter -> S.Seq Todo -> S.Seq Todo
filterTodo FilterNone = id
filterTodo FilterCompleted = S.filter (view todoCompleted)
filterTodo FilterActive = S.filter (not . view todoCompleted)

-- | Update the input box
updateCurrentInput :: STMMailbox JSString -> JSString -> Message ()
updateCurrentInput mb input = modifyMailbox mb (const input)

-- | Submit the todo form and add a new todo item
submitTodo :: STMMailbox (S.Seq Todo) -> JSString -> Message ()
submitTodo todoMb input = if JS.null input
                            then return ()
                            else addTodo todoMb input

todoMVC :: STMMailbox JSString -> STMMailbox TodoFilter -> STMMailbox (S.Seq Todo) -> LiveVDom
todoMVC inputMb filterMb todoListMb = [valentine|
<div>
  <section id="todoapp">
    <header id="header">
      <h1>
        todos
      ${todoForm todoListMb inputMb $ addTodo todoListMb}
    ${todoBody todoListMb filterMb}
    !{todoBodyFooter (sendMessage $ snd filterMb) (clearCompleted todoListMb) <$> fst todoListMb}
  ${todoFooter}
|]

todoForm :: STMMailbox (S.Seq Todo) -> STMMailbox JSString -> (JSString -> Message ()) -> LiveVDom
todoForm todoMb inputMb update = T.LiveVNode [submitEvent] "form" [prop] children
  where submitEvent = submit $ \e -> do
          preventDefault e
          let jse = unsafeCoerce e :: JSVal
          input <- [js| `jse.currentTarget[0].value |]
          [js_| `jse.currentTarget[0].value = "" |]
          runMessages $ submitTodo todoMb input
        prop = Property "id" $ JSPString "todo-form"
        children = S.singleton $ T.LiveVNode [] "input" props S.empty
        props = [Property "id" $ JSPString "new-todo"
                , Property "placeholder" $ JSPString "What needs to be done?"
                , Property "autofocus" $ JSPBool True]


input :: [Property] -> LiveVDom
input props = T.LiveVNode [] "input" props S.empty

inputCheckboxToggleAll :: Message () -> LiveVDom
inputCheckboxToggleAll onClick = T.addEvent (click $ const $ runMessages onClick) $ input checkboxProps
  where checkboxProps = [Property "id" $ JSPString "toggle-all"
                        ,Property "type" $ JSPString "checkbox"
                        ]

todoBody :: STMMailbox (S.Seq Todo) -> STMMailbox TodoFilter -> LiveVDom
todoBody todoMb@(todoEnv, todoAddr) (filternEnv,_) = [valentine|
<section id="main">
  ${inputCheckboxToggleAll (toggleAll todoMb)}
  <label for="toggle-all">
    Mark all as complete
  <ul id="todo-list">
    &{forEach (filterTodo <$> filternEnv <*> todoEnv, todoAddr) displayTodoItem }
|]

displayTodoItem :: Todo -> (Maybe Todo -> Message ()) -> LiveVDom
displayTodoItem item updateItem = (flip T.addProps $ editing ++ completed)[valentine|
<li>
  <div class="view">
    ${T.addEvent (click . const . void . runMessages . updateItem . Just . toggleCompleted' $ item) $ input (liProps)}
    ${todoItemTitle}
    ${buttonWith (updateItem Nothing) [Property "class" $ JSPString "destroy"] ""}
  <form ng-submit="saveEdits(todo, 'submit')">
    <input class="edit" ng-trim="false" ng-model="todo.title" todo-escape="revertEdits(todo)" ng-blur="saveEdits(todo, 'blur')" todo-focus="todo == editedTodo">
|]
  where todoItemTitle = T.addEvent (dblclick . const . void . runMessages . updateItem . Just $ setEdit item) [valentine|
          <label>
            ^{item ^. todoTitle}
        |]
        liProps = checked ++ completed ++ [
                    Property "class" $ JSPString "toggle"
                  , Property "type" $ JSPString "checkbox"]
        checked = [Property "checked" $ JSPBool $ item ^. todoCompleted]
        completed = if item ^. todoCompleted
                      then [Property "class" $ JSPString "completed"]
                      else []
        editing = []
        toggleCompleted' = over todoCompleted not

todoBodyFooter :: (TodoFilter -> Message ()) -> Message () -> S.Seq Todo -> LiveVDom
todoBodyFooter updateFilter clearTodos todoItems = [valentine|
<footer id="footer">
  <span id="todo-count">
    <strong>
      ${todoItemsCount $ S.length todoItems}
  <ul id="filters">
    ${filterOption "All" $ updateFilter FilterNone }
    ${filterOption "Active" $ updateFilter FilterActive }
    ${filterOption "Completed" $ updateFilter FilterCompleted }
|]
-- ${buttonWith clearTodos [] "Clear completed"}

filterOption :: JSString -> Message () -> LiveVDom
filterOption title onClick = T.addEvent (click $ const $ void $ runMessages onClick) [valentine|
<li>
  <a class="">
    ^{title}
|]

todoItemsCount :: Int -> LiveVDom
todoItemsCount 1 = [valentine| 1 item left |]
todoItemsCount x = [valentine| ^{append (pack $ show x) " items left"}|]


-- | Static footer for the page
todoFooter :: LiveVDom
todoFooter = [valentine|
<footer id="info">
  <p>
    Double-click to edit a todo
  <p>
    ^{"Part of              "}
    <a href="http://todomvc.com">
      TodoMVC
|]
