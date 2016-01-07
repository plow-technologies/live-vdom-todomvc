{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Types
import           View

import           Control.Concurrent.STM.Notify
import qualified Data.Sequence                 as S
import           LiveVDom

import           GHCJS.Foreign.QQ
import Data.JSString

jsFiles, cssFiles :: [JSString]
jsFiles = ["lib/base.js"]
cssFiles = ["lib/base.css"]



addCss :: JSString -> IO ()
addCss str = [js_|
  var ss = document.createElement("link");
  ss.type = "text/css";
  ss.rel = "stylesheet";
  ss.href = `str;
  document.getElementsByTagName("head")[0].appendChild(ss);
|]

addJs :: JSString -> IO ()
addJs str = [js_|
  var js = document.createElement("script");
  js.type = "text/javascript";
  js.src = `str;
  document.getElementsByTagName("head")[0].appendChild(js);
|]

main :: IO ()
main = do
  mapM_ addCss cssFiles
  mapM_ addJs jsFiles
  xs <- spawnIO S.empty
  fil <- spawnIO FilterNone
  inp <- spawnIO ""
  container <- createContainer
  runDom container (return ()) (todoMVC inp fil xs)
