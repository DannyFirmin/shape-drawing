--
-- COMP1100/1130, Semester 1, 2018
--

module ColourName where

import           CodeWorld

data ColourName
  = Magenta
  | Black
  | Green
  | Yellow
  | Orange
  | Cyan
  deriving (Show)

--
-- | Black Colour Test
--
-- >>> colourNameToColour Black
-- RGBA 0.0 0.0 0.0 1.0

--
-- | Green Colour Test
--
-- >>> colourNameToColour Green
-- RGBA 0.125 0.875 0.125 1.0

--
-- | Orange Colour Test
--
-- >>> colourNameToColour Orange
-- RGBA 0.875 0.5 0.125 1.0

colourNameToColour :: ColourName -> Colour
colourNameToColour colour = case colour of
  Magenta -> magenta
  --RGBA 255.0 0.0 255.0 1.0
  Black -> black
  --RGBA 0.0 0.0 0.0 1.0
  Green -> green
  --RGBA 0.125 0.875 0.125 1.0
  Yellow -> yellow
  --RGBA 255.0 255.0 0.0 1.0
  Orange -> orange
  --RGBA 0.875 0.5 0.125 1.0
  Cyan -> cyan
  --RGBA 0 184 212 1


colourKeyMap :: [(String, ColourName)]
colourKeyMap =
    [("M", Magenta),
     ("B", Black),
     ("G", Green),
     ("Y", Yellow),
     ("O", Orange),
     ("C", Cyan)]
