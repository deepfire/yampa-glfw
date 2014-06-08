{-# LANGUAGE BangPatterns #-}

-- Copyright   :  (c) Kosyrev Serge 2014
-- License     :  GNU GPLv3 (see COPYING)
-- Heavily based on yampa-glut by Nikolay Orlyuk

module FRP.Yampa.GLFW.InternalUI where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

data UI = GlfwWindowClose
        | GlfwWindowResize !Size
        | GlfwRedraw
        | GlfwChar !Char !Bool
        | GlfwKey !Key !Bool
        | GlfwMouseButton !MouseButton !Bool
        | GlfwMousePosition !Position
        | GlfwMouseWheel !Int
    deriving (Eq, Show)
