{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Lib
import           Types

import           Control.Concurrent.STM.Notify
import qualified Data.Sequence                 as S
import           LiveVDom

import           GHCJS.Foreign.QQ


jsFiles = ["../../../../lib/base.js"]
cssFiles = ["../../../../lib/base.css"]



addCss :: String -> IO ()
addCss str = [js_|
  var ss = document.createElement("link");
  ss.type = "text/css";
  ss.rel = "stylesheet";
  ss.href = `str;
  console.log(ss);
  document.getElementsByTagName("head")[0].appendChild(ss);
|]

addJs :: String -> IO ()
addJs str = [js_|
  var js = document.createElement("script");
  js.type = "text/javascript";
  js.src = `str;
  console.log(js);
  document.getElementsByTagName("head")[0].appendChild(js);
|]


main :: IO ()
main = do
  mapM_ addJs jsFiles
  mapM_ addCss cssFiles
  xs <- spawnIO S.empty
  fil <- spawnIO FilterNone
  inp <- spawnIO ""
  container <- createContainer
  runDom container (return ()) (todoMVC inp fil xs)
