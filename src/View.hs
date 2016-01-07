{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module View where

import           Lib
import           Types

import           Control.Lens           (over, (^.))
import           Control.Monad          (void)
import qualified Data.Sequence          as S


import           Data.JSString          as JS
import           GHCJS.Foreign.QQ       (js, js_)
import           GHCJS.Types            (JSVal)
import           GHCJS.VDOM.Event       (click, dblclick, preventDefault,
                                         submit)
import           LiveVDom
import           LiveVDom.Adapter.Types
import qualified LiveVDom.Types         as T
import           Valentine

-- Hopefully this can be removed
import           Unsafe.Coerce (unsafeCoerce)


todoMVC :: STMMailbox JSString -> STMMailbox TodoFilter -> STMMailbox (S.Seq Todo) -> LiveVDom
todoMVC inputMb filterMb todoListMb = [valentine|
<div>
  <section id="todoapp">
    <header id="header">
      <h1>
        todos
      ${todoForm todoListMb}
    ${todoBody todoListMb filterMb}
    !{todoBodyFooter (sendMessage $ snd filterMb) (clearCompleted todoListMb) <$> fst filterMb <*> fst todoListMb}
  ${todoFooter}
|]

todoForm :: STMMailbox (S.Seq Todo) -> LiveVDom
todoForm todoMb = T.LiveVNode [submitEvent] "form" [prop] children
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



-- | Display a todo single item
-- Currently doesn't have the input form finished
displayTodoItem :: Todo -> (Maybe Todo -> Message ()) -> LiveVDom
displayTodoItem item updateItem = (flip T.addProps $ completed)[valentine|
<li>
  <div class="view">
    ${T.addEvent (click . const . void . runMessages . updateItem . Just . toggleCompleted' $ item) $ input (liProps)}
    ${todoItemTitle}
    ${buttonWith (updateItem Nothing) [Property "class" $ JSPString "destroy"] ""}
  <form>
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
        toggleCompleted' = over todoCompleted not


todoBodyFooter :: (TodoFilter -> Message ()) -> Message () -> TodoFilter -> S.Seq Todo -> LiveVDom
todoBodyFooter updateFilter clearTodos selected todoItems = [valentine|
<footer id="footer">
  <span id="todo-count">
    <strong>
      ${todoItemsCount $ S.length todoItems}
  <ul id="filters">
    ${filterOption "All" selected FilterNone updateFilter }
    ${filterOption "Active" selected FilterActive updateFilter }
    ${filterOption "Completed" selected FilterCompleted updateFilter }
  ${buttonWith clearTodos [Property "id" $ JSPString "clear-completed"] "Clear completed"}
|]
--

filterOption :: JSString -> TodoFilter -> TodoFilter -> (TodoFilter -> Message ()) -> LiveVDom
filterOption title selected option onClick = [valentine|
<li>
  ${innerA}
|]
  where innerA = T.LiveVNode [clickEvent] "a" propsList $ S.singleton $ T.StaticText [] title
        clickEvent = click $ const $ void $ runMessages $ onClick option
        propsList = if selected == option
                      then [Property "class" $ JSPString "selected"]
                      else []

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
