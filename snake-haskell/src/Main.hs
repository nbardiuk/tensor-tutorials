{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (unless)
import           Data.Maybe    (catMaybes, listToMaybe)
import           Draw
import           Game
import           SDL
import           System.Random

main :: IO ()
main = do
  let height = 20
      width = 20

  initialize [InitVideo]
  window   <- createWindow "Snake" defaultWindow
  windowSize window $= V2 (toEnum $ toCoord width) (toEnum $ toCoord height)
  renderer <- createRenderer window (-1) defaultRenderer

  stdGen   <- getStdGen
  lastTime <- time
  let game = newGame width height stdGen
  appLoop renderer lastTime game

  destroyRenderer renderer
  destroyWindow window
  quit

appLoop :: Renderer -> Double -> Game -> IO ()
appLoop renderer lastTime game = do
  events  <- pollEvents
  newTime <- time
  let eventIsQPress event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          keyboardEventKeyMotion keyboardEvent
            == Pressed
            && keysymKeycode (keyboardEventKeysym keyboardEvent)
            == KeycodeQ
        _ -> False
      qPressed = any eventIsQPress events
      pressed event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          if keyboardEventKeyMotion keyboardEvent == Pressed
            then toKey $ keysymKeycode $ keyboardEventKeysym keyboardEvent
            else Nothing
        _ -> Nothing
      key = listToMaybe $ catMaybes $ pressed <$> events

  let nextGame = update ( keyPressed game key ) (newTime - lastTime)

  drawGame renderer nextGame

  unless qPressed (appLoop renderer newTime nextGame)

toKey :: Keycode -> Maybe Key
-- arrows
toKey KeycodeUp    = Just KUp
toKey KeycodeLeft  = Just KLeft
toKey KeycodeRight = Just KRight
toKey KeycodeDown  = Just KDown
-- wasd
toKey KeycodeW     = Just KUp
toKey KeycodeS     = Just KDown
toKey KeycodeA     = Just KLeft
toKey KeycodeD     = Just KRight
-- vim
toKey KeycodeK     = Just KUp
toKey KeycodeJ     = Just KDown
toKey KeycodeH     = Just KLeft
toKey KeycodeL     = Just KRight
--
toKey _            = Nothing
