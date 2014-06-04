
-- Copyright   :  (c) Kosyrev Serge 2014
-- License     :  GNU GPLv3 (see COPYING)
-- Based on yampa-glut by Nikolay Orlyuk

module FRP.Yampa.GLFW.UI
    ( UI
    , resized, windowResize, redraw
    , mousePosition, simpleMousePosition
    , keyAction, mouseButtonAction
    , keyPress, keyPressed, mouseButtonPressed
    ) where

import Control.Arrow

import FRP.Yampa (SF, hold)
import FRP.Yampa.Event
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Size(..), Position(..), GLfloat, GLdouble)

import FRP.Yampa.GLFW.InternalUI

import Graphics.UI.GLFW

-- | Re-display request from GLFW
redraw :: SF (Event UI) (Event ())
redraw = arr $ tagWith () . filterE (GlfwRedraw==)

-- | Re-shape request from GLFW
resized :: SF (Event UI) (Event Size)
resized = arr (mapFilterE f) where
    f (GlfwWindowResize sz) = Just sz
    f _ = Nothing

-- | Window size
windowResize :: SF (Event UI) Size
windowResize = hold (Size 1 1) <<< resized

-- | Latest mouse position in window
mousePosition :: SF (Event UI) Position
mousePosition = hold (Position 0 0) <<< arr (mapFilterE f) where
    f (GlfwMousePosition posn) = Just posn
    f _ = Nothing

-- | Latest mouse position in window with simple coord transform (i.e. unit)
simpleMousePosition :: Fractional a => SF (Event UI) (GL.Vector2 a)
simpleMousePosition = windowResize &&& mousePosition >>> arr f where
    f (Size w h, Position x y) = GL.Vector2 x' y' where
        {-
        b = fromIntegral (w `min` h)
        x' = (2 * fromIntegral x - fromIntegral w) / b
        y' = (fromIntegral h - 2 * fromIntegral y) / b
        -}
        b = realToFrac (w `min` h)
        x' = (2 * realToFrac x - realToFrac w) / b
        y' = (realToFrac h - 2 * realToFrac y) / b

{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (GL.Vector2 GLfloat) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (GL.Vector2 GLdouble) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (GL.Vector2 Float) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (GL.Vector2 Double) #-}


-- | Key action events
keyAction :: SF (Event UI) (Event (Bool, Either Char Key))
keyAction = arr (mapFilterE f) where
    f (GlfwChar c ks) = Just (ks, Left c)
    f (GlfwKey  k ks) = Just (ks, Right k)
    f _ = Nothing

-- | Mouse buttons action events
mouseButtonAction :: SF (Event UI) (Event (Bool, MouseButton))
mouseButtonAction = arr (mapFilterE f) where
    f (GlfwMouseButton mb ks) = Just (ks, mb)
    f _ = Nothing

-- | State of modifiers associated with keyboard/mouse event
-- modifiers :: SF (Event UI) (Event Modifiers)
-- modifiers = arr (mapFilterE f) where
--     f (GlutKeyboardMouse _ _ m _) = Just m
--     f _ = Nothing

-- | Key press events
keyPress :: SF (Event UI) (Event (Either Char Key))
keyPress = keyAction >>^ fmap snd . filterE ((==True) . fst)

-- | Key pressed state for specific key
keyPressed :: Either Char Key -> SF (Event UI) Bool
keyPressed key = hold False <<< mapFilterE f ^<< keyAction where
    f (x, key') | key == key' = Just (x == True)
    f _ = Nothing

-- | Mouse button pressed state for specific button
mouseButtonPressed :: MouseButton -> SF (Event UI) Bool
mouseButtonPressed button = hold False <<< mapFilterE f ^<< mouseButtonAction where
    f (x, button') | button == button' = Just (x == True)
    f _ = Nothing

-- | Crossing/leaving event
-- crossing :: SF (Event UI) (Event Crossing)
-- crossing = arr (mapFilterE f) where
--     f (GlutCrossing c) = Just c
--     f _ = Nothing
