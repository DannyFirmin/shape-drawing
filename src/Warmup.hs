--
-- COMP1100/1130, Semester 1, 2018
--

module Main where
import CodeWorld

myPicture :: Picture
myPicture = colored red (translated 3.5 3.5 (solidRectangle 2.5 2.5))

yourPicture :: Picture
yourPicture = translated 3 3 (circle 2)

hisPicture :: Picture
hisPicture = translated (-3) 6 (rectangle 4 4)

ourPicture :: Picture
ourPicture = myPicture & yourPicture & hisPicture
main :: IO ()
main = drawingOf ourPicture