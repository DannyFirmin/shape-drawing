--
-- COMP1100/1130, Semester 1, 2018
--
module State where


import ColourName
import Graphic
import Shape
import CodeWorld (Point)

data State = World [Graphic]
                   Tool
                   ColourName
  deriving (Show)

initialTool :: Tool
initialTool = RectangleTool Nothing

initialColour :: ColourName
initialColour = Black

initialState :: State
initialState = World [] initialTool initialColour

changeTool ::State -> Tool -> State
changeTool (World g _ c) t' = (World g t' c)

passColour :: State -> ColourName -> State
passColour (World g t _) c' = (World g t c')

passStartPoint :: State -> Maybe Point -> State
passStartPoint (World g t c) (Just p) =
  case t of
  RectangleTool Nothing -> World g (RectangleTool (Just p)) c
  EllipseTool Nothing -> World g (EllipseTool (Just p)) c
  LineTool Nothing -> World g (LineTool (Just p)) c
  PolygonTool [] -> World g (PolygonTool (p:[])) c
  PolygonTool (x:xs) -> World g (PolygonTool (p:x:xs)) c
  _ -> World g t c
passStartPoint (World g t c) Nothing = World g t c


removeShape :: State -> State
removeShape (World [x] t c) = (World [] t c)
removeShape (World (x:xs) t c) = (World (xs) t c)
removeShape (World [] t c) = (World [] t c)