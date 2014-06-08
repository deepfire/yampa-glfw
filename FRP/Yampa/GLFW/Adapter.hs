{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PackageImports #-}

-- Copyright   :  (c) Kosyrev Serge 2014
-- License     :  GNU GPLv3 (see COPYING)
-- Heavily based on yampa-glut by Nikolay Orlyuk

module FRP.Yampa.GLFW.Adapter
    ( adaptSimple, adapt, simpleInit
    , Action, Reaction
    , actionIO, actionExit
    , module FRP.Yampa.GLFW.UI
    ) where

import Control.Arrow
import Control.Monad
import Control.Newtype
import Data.IORef
import Data.Monoid

import qualified "GLFW-b" Graphics.UI.GLFW  as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import FRP.Yampa (SF, reactInit, react)
import FRP.Yampa.Event

import FRP.Yampa.GLFW.InternalUI
import FRP.Yampa.GLFW.UI

import Unsafe.Coerce

-- | Adapter to connect @FRP.Yampa@ with @Graphics.UI.GLFW@ and does
-- @simpleInit@.
adaptSimple :: String -> Reaction -> IO ()
adaptSimple title sf = simpleInit title >> adapt sf

-- | Adapter to connect @FRP.Yampa@ with @Graphics.UI.GLFW@. Assumes that
-- GLFW have been initialized.
adapt :: Reaction -> IO ()
adapt sf = do
    timeRef   <- newIORef (0.0 :: Double)
    closeFlag <- newIORef False

    let rInit = return NoEvent
        rActuate _ _ NoEvent = return False
        rActuate _ _ (Event (ActionExit io)) = io >> return True
        rActuate _ _ (Event (ActionIO io)) = io >> return False

    rh <- reactInit rInit rActuate sf

    let reactEvent ev = do
            time <- readIORef timeRef
            time' <- GLFW.getTime
            writeIORef timeRef time'
            let dt = (time' - time)
            b <- react rh (dt, Just (Event ev))
            if b then writeIORef closeFlag True
                 else return ()

    -- set callbacks
    GLFW.setWindowCloseCallback $ do
                  writeIORef closeFlag True
                  return True
    GLFW.setWindowSizeCallback $ (\ w h -> reactEvent $ GlfwWindowResize $ GL.Size (unsafeCoerce w) (unsafeCoerce h))
    GLFW.setMouseButtonCallback $ (\ btn st -> reactEvent $ GlfwMouseButton btn st)
    GLFW.setMousePositionCallback $ (\ x y -> reactEvent $ GlfwMousePosition $ GL.Position (unsafeCoerce x) (unsafeCoerce y))
    GLFW.setKeyCallback $ (\ ch st -> reactEvent $ GlfwKey ch st)

    let loop' = do
             reactEvent GlfwRedraw
             closep <- readIORef closeFlag
             unless closep loop'
    loop'

-- | Simple initialization of GLFW
simpleInit :: String -> IO Bool
simpleInit title = do
    success <- GLFW.initialize
    if success
       then do
         _ <- GLFW.openWindow GLFW.defaultDisplayOptions
                 { GLFW.displayOptions_width              = 1024
                 , GLFW.displayOptions_height             = 768
                 }

         GLFW.setWindowTitle title
         return True
       else return False

-- | Action to perform in response to something
data Action = ActionExit (IO ())
            | ActionIO (IO ())

-- | Simple IO action that do not control mainLoop life-time
actionIO :: IO () -> Action
actionIO = ActionIO

-- | Terminate mainLoop action
actionExit :: Action
actionExit = ActionExit (return ())

-- | Top level reaction signal function
type Reaction = SF (Event UI) (Event Action)

-- Monoid instances to combine actions, reactions etc
instance Newtype Action (IO ()) where
    pack = ActionIO
    unpack (ActionIO x) = x
    unpack (ActionExit x) = x

instance Monoid Action where
    mempty = ActionIO (return ())
    a@(ActionExit _) `mappend` _ = a
    (ActionIO a) `mappend` (ActionExit b) = ActionExit (a >> b)
    (ActionIO a) `mappend` (ActionIO b) = ActionIO (a >> b)

instance Monoid a => Monoid (Event a) where
    mempty = Event mempty
    NoEvent `mappend` b' = b'
    Event a `mappend` b' = Event (a `mappend` f b') where
        f NoEvent = mempty
        f (Event b) = b

instance Monoid b => Monoid (SF a b) where
    mempty = arr mempty
    sfX `mappend` sfY = (sfX &&& sfY) >>^ uncurry mappend

