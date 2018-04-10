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

-- JK: You are repeating yourself a lot here. A `where` clause would
-- simplify things a lot (or even pulling the common computations into
-- `getWidthHeightShift`).
--
-- Note that the marking scheme for Part A only marks
-- functionality. When I mark the full assignment, it will consider
-- style too.
--
-- Also, your calculations aren't quite right. Consider a
-- rectangle. `shapeToPic` will drop it with its midpoint the origin
-- (0, 0). The shift you want will move the midpoint of the rectangle
-- to (mid x1 x2, mid y1 y2), where mid is a function to calculate
-- halfway between its two arguments. What does that function look
-- like?
getRectangleGraphic :: Point -> Point -> ColourName -> Graphic
getRectangleGraphic (x1,y1) (x2,y2) c= Graphic (Rectangle (abs(x2-x1))(abs(y2-y1))) c (x2-abs(x2-x1)/2,y2-abs(y2-y1)/2)

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic (x1,y1) (x2,y2) c= Graphic (Ellipse (abs(x2-x1)/2)(abs(y2-y1)/2)) c (x2-abs(x2-x1)/2,y2-abs(y2-y1)/2)

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic a b c = Graphic (Line a b) c (0,0)

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic a c = Graphic (Polygon a) c (0,0)

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift = undefined -- TODO

shapeToPic :: Shape -> Picture
shapeToPic shape = case shape of
  Rectangle x y -> solidRectangle x y
  Ellipse x y -> scaled x y (solidCircle 1)
  Polygon x -> solidPolygon x
  Line x y -> polyline [x,y]

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic (Graphic shape colourName (x,y)) = coloured (colourNameToColour colourName) (translated x y (shapeToPic shape))
