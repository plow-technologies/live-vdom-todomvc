{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Lib where

import           Types

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.Sequence          as S


import           Data.JSString
import           GHCJS.VDOM.Event
import           LiveVDom
import           LiveVDom.Adapter.Types
import qualified LiveVDom.Types         as T
import           Valentine

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
submitTodo :: STMMailbox (S.Seq Todo) -> STMMailbox JSString -> Message ()
submitTodo todoMb (inputEnv, inputAddr) = addTodo todoMb =<< recvMessage inputEnv

todoMVC :: STMMailbox JSString -> STMMailbox TodoFilter -> STMMailbox (S.Seq Todo) -> LiveVDom
todoMVC inputMb filterMb todoListMb = [valentine|
<script type="text/ng-template" id="todomvc-index.html">
  <section id="todoapp">
    <header id="header">
      <h1>
        todos
      ${todoForm (fst inputMb) $ addTodo todoListMb}
    ${todoBody}
    !{todoBodyFooter (sendMessage $ snd filterMb) (clearCompleted todoListMb) <$> fst todoListMb}
  ${todoFooter}
|]

todoForm :: STMEnvelope JSString -> (JSString -> Message ()) -> LiveVDom
todoForm inputEnv update = T.LiveVNode [submitEvent] "form" [prop] children
  where submitEvent = submit $ const $ void $ runMessages $ return ()
        prop = Property "id" $ JSPString "todo-form"
        children = S.singleton $ T.LiveVNode [] "input" props S.empty
        props = [Property "id" $ JSPString "new-todo"
                , Property "placeholder" $ JSPString "What needs to be done?"
                , Property "autofocus" $ JSPBool True]
-- Still needs input hooked up




todoBody :: LiveVDom
todoBody = [valentine|
<section id="main">
  <input id="toggle-all" type="checkbox" ng-model="allChecked" ng-click="markAll(allChecked)">
  <label for="toggle-all">
    Mark all as complete
  <ul id="todo-list">
    <li ng-repeat="todo in todos | filter:statusFilter track by $index" ng-class="{completed: todo.completed, editing: todo == editedTodo}">
|]

displayTodoItem :: Todo -> (Maybe Todo -> Message ()) -> LiveVDom
displayTodoItem item updateItem = [valentine|
<div>
  <div class="view">
    <input class="toggle" type="checkbox" ng-model="todo.completed" ng-change="toggleCompleted(todo)">
    ${todoItemTitle}
    ${buttonWith (updateItem Nothing) [Property "class" $ JSPString "destroy"] ""}
  <form ng-submit="saveEdits(todo, 'submit')">
    <input class="edit" ng-trim="false" ng-model="todo.title" todo-escape="revertEdits(todo)" ng-blur="saveEdits(todo, 'blur')" todo-focus="todo == editedTodo">
|]
  where todoItemTitle = T.addEvent (dblclick . const . void . runMessages . updateItem . Just $ setEdit item) [valentine|
<label>
  ^{item ^. todoTitle}
|]

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
  ${buttonWith clearTodos [] "Clear completed"}
|]

filterOption :: JSString -> Message () -> LiveVDom
filterOption title onClick = T.addEvent (submit $ const $ void $ runMessages onClick) [valentine|
<li>
  <a>
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
    Part of
    <a href="http://todomvc.com">
    TodoMVC
|]
