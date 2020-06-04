module Ray.Events (handle) where

import Data.Foldable (for_)
import Debug.Trace (traceM)
import System.Exit (exitSuccess)

import qualified SDL

handle :: IO Bool
handle = do
  events <- SDL.pollEvents
  for_ events $ \e -> case SDL.eventPayload e of
    SDL.WindowShownEvent eventData ->
      onWindowShown eventData
    SDL.WindowSizeChangedEvent eventData ->
      onWindowSizeChanged eventData
    SDL.KeyboardEvent eventData ->
      onKeyboard eventData
    _ ->
      pure ()
  pure $ SDL.QuitEvent `elem` map SDL.eventPayload events

onKeyboard :: SDL.KeyboardEventData -> IO ()
onKeyboard SDL.KeyboardEventData{..} =
  case keyboardEventKeyMotion of
    SDL.Pressed ->
      onKeyDown keyboardEventKeysym
    SDL.Released ->
      onKeyUp keyboardEventKeysym

onWindowShown :: SDL.WindowShownEventData -> IO ()
onWindowShown SDL.WindowShownEventData{..} = do
  size <- SDL.glGetDrawableSize windowShownEventWindow
  traceM $ "Window shown: " <> show size

onWindowSizeChanged :: SDL.WindowSizeChangedEventData -> IO ()
onWindowSizeChanged SDL.WindowSizeChangedEventData{..} =
  traceM $ "Window resized: " <> show windowSizeChangedEventSize

onKeyDown :: SDL.Keysym -> IO ()
onKeyDown SDL.Keysym{..} = do
  case keysymKeycode of
    SDL.KeycodeEscape -> do
      SDL.quit
      exitSuccess
    _ ->
      pure ()

onKeyUp :: SDL.Keysym -> IO ()
onKeyUp SDL.Keysym{..} =
  pure ()
