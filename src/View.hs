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
getRectangleGraphic (x1,y1) (x2,y2) c= Graphic (Rectangle (abs(x2-x1))(abs(y2-y1))) c (0,0)


getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic (x1,y1) (x2,y2) c= Graphic (Ellipse (abs(x2-x1))(abs(y2-y1))) c (0,0)

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic a b c = Graphic (Line a b) c (0,0)

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic a c = Graphic (Polygon a) c (0,0)

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift = undefined -- TODO

shapeToPic :: Shape -> Picture
shapeToPic shape = case shape of
  Rectangle x y -> rectangle x y
  Ellipse x y -> scaled x y (circle x)
  Polygon x -> polygon x
  Line x y -> polyline [x,y]

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic (Graphic shape colourName point) = coloured (colourNameToColour colourName) (translated 0 0 (shapeToPic shape))