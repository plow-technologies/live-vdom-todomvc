{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Data.JSString

data Todo = Todo {
  _todoTitle     :: JSString
, _todoCompleted :: Bool
, _todoEdit      :: Bool
, _todoShow      :: Bool
} deriving (Eq, Show)

makeLenses ''Todo


data TodoFilter = FilterNone
                | FilterCompleted
                | FilterActive
 deriving (Eq, Show)
makePrisms ''TodoFilter

