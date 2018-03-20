--
-- COMP1100/1130, Semester 1, 2018
--
module View where

import CodeWorld
import ColourName
import Graphic
import Shape
import State

import qualified Data.Text as Text

drawState :: State -> Picture
drawState (World gs tool colour) =
  pictures $ shapeToPicture : colourToPicture : graphicsToPics gs
  where
    shapeToPicture, colourToPicture :: Picture
    shapeToPicture =
      translated (-13.5) 8 $ (text . Text.pack) ("Shape: " ++ shapeToText)
    colourToPicture =
      translated (-13.5) 7 $ (text . Text.pack) ("Colour: " ++ colourToText)
    shapeToText :: String
    shapeToText =
      let shape = takeWhile (/= ' ') $ show tool
       in take (length shape - 4) shape
    colourToText :: String
    colourToText = show colour

--
-- | drawNewGraphic Test 1
--
-- >>> drawNewGraphic (World [] (RectangleTool (Just (-3, 3))) Black) (Just (3, -3))
-- World [Graphic (Rectangle 6.0 6.0) Black (0.0,0.0)] (RectangleTool Nothing) Black
-- 
-- | drawNewGraphic Test 2
-- 
-- >>> drawNewGraphic (World [] (RectangleTool (Just (0, 0))) Orange) (Just (3, 7))
-- World [Graphic (Rectangle 3.0 7.0) Orange (1.5,3.5)] (RectangleTool Nothing) Orange
drawNewGraphic :: State -> Maybe Point -> State
drawNewGraphic = undefined -- TODO

getNewGraphic :: State -> Maybe Point -> Maybe Graphic
getNewGraphic = undefined -- TODO

getRectangleGraphic :: Point -> Point -> ColourName -> Graphic
getRectangleGraphic = undefined -- TODO

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic = undefined -- TODO

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic = undefined -- TODO

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic = undefined -- TODO

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift = undefined -- TODO

shapeToPic :: Shape -> Picture
shapeToPic line = case line of
  Line -> polyline(point,point)
  _ -> undefined
  -- what does line shape and polyline mean?
  -- What cases do it have? I don't understand the question

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic = undefined -- TODO
