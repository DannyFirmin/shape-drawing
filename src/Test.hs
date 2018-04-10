module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/ColourName.hs"
  , "src/Events.hs"
  , "src/Graphic.hs"
  , "src/View.hs"
  , "src/Shape.hs"
  , "src/State.hs"
  ]
