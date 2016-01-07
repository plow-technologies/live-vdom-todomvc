{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Types

import           Control.Lens  (each, ix, over, set, view)
import qualified Data.Sequence as S


import           Data.JSString as JS
import           LiveVDom      (Message, STMMailbox, modifyMailbox)

-- | Make a Todo item from a title
mkTodo :: JSString -> Todo
mkTodo title = Todo title False False True

-- | Add a Todo item the the front of a sequence
addTodo :: STMMailbox (S.Seq Todo) -> JSString -> Message ()
addTodo mb title = modifyMailbox mb appendNew
  where appendNew = (S.|> mkTodo title)

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
