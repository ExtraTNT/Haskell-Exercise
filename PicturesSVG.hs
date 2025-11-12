-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- Updated by Sven Hugi
-- (c) Sven Hugi, 2025 - 2025
--
-- 	PicturesSVG
--
--      The Pictures functionality implemented by translation
--      SVG (Scalable Vector Graphics)
--
--      These Pictures could be rendered by conversion to ASCII art,
--      but instead are rendered into SVG, which can then be viewed in
--      a browser: firefox does a good job.
--
-----------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module PicturesSVG where

import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode) )
import GHC.IO.Handle.Internals (withAllHandles__)
-- Pictures represened by a type of trees, so this is a deep
-- embedding.

data Picture
 = Img Image
 | Above Picture Picture
 | Beside Picture Picture
 | FlipH Picture
 | FlipV Picture
 | Negative Picture
   deriving (Show)

-- Coordinates are pairs (x,y) of integers
--
--  o------> x axis
--  |
--  |
--  V
--  y axis


type Point = (Int,Int)

-- The Point in an Image gives the dimensions of the image in pixels.

data Image = Image Name Point
             deriving (Show)

data Name  = Name String
             deriving (Show)

--
-- The functions over Pictures
--

above, beside :: Picture -> Picture -> Picture

above  = Above
beside = Beside

-- flipH is flip in a horizontal axis
-- flipV is flip in a vertical axis
-- negative negates each pixel
-- rotate180 rotates a picture 180°

-- The definitions of flipH, flipV, negative push the
-- constructors through the binary operations to the images
-- at the leaves.

-- Original implementation incorrect: it pushed the
-- flipH and flipV through all constructors ...
-- Now it distributes appropriately over Above, Beside and Over.

flipH, flipV, invert, rotate180 :: Picture -> Picture

flipH (Above pic1 pic2)  = flipH pic2 `Above` flipH pic1
flipH (Beside pic1 pic2) = flipH pic1 `Beside` flipH pic2
flipH pic                = FlipH pic

flipV (Above pic1 pic2)  = flipV pic1 `Above` flipV pic2
flipV (Beside pic1 pic2) = flipV pic2 `Beside` flipV pic1
flipV pic                = FlipV pic

invert = Negative

rotate180 = flipH . flipV

unpackX, unpackY :: [Picture] -> Picture
unpackX [x] = x
unpackX (x:xs) = beside x $ unpackX xs

unpackY [x] = x
unpackY (x:xs) = above (unpackY xs) x

stackN :: Picture -> Int -> [Picture]
stackN p n = take n $ infinite p

-- Library functions

-- Dimensions of pictures
width,height :: Picture -> Int

width (Img (Image _ (x,_))) = x
width (Above pic1 pic2)     = max (width pic1) (width pic2)
width (Beside pic1 pic2)    = width pic1 + width pic2
width (FlipH pic)           = width pic
width (FlipV pic)           = width pic
width (Negative pic)        = width pic

height (Img (Image _ (x,y))) = y
height (Above pic1 pic2)     = height pic1 + height pic2
height (Beside pic1 pic2)    = max (height pic1) (height pic2)
height (FlipH pic)           = height pic
height (FlipV pic)           = height pic
height (Negative pic)        = height pic

-- Converting pictures to a list of basic images.

-- A Filter represents which of the actions of flipH, flipV
-- and negative is to be applied to an image in forming a
-- Basic picture.

data Filter = Filter {fH, fV, neg :: Bool}
              deriving (Show)

newFilter :: Filter
newFilter = Filter False False False

data Basic = Basic Image Point Filter
             deriving (Show)

-- Flatten a picture into a list of Basic pictures.
-- The Point argument gives the origin for the coversion of the
-- argument.

flatten :: Point -> Picture -> [Basic]

flatten (x,y) (Img image)        = [Basic image (x,y) newFilter]
flatten (x,y) (Above pic1 pic2)  = flatten (x,y) pic1 ++ flatten (x, y + height pic1) pic2
flatten (x,y) (Beside pic1 pic2) = flatten (x,y) pic1 ++ flatten (x + width pic1 , y) pic2
flatten (x,y) (FlipH pic)        = map flipFH $ flatten (x,y) pic
flatten (x,y) (FlipV pic)        = map flipFV $ flatten (x,y) pic
flatten (x,y) (Negative pic)     = map flipNeg $ flatten (x,y) pic

-- flip one of the flags for transforms / filter

flipFH :: Basic -> Basic
flipFH (Basic img (x,y) f@(Filter {fH=boo}))   = Basic img (x,y) f{fH = not boo}
flipFV :: Basic -> Basic
flipFV (Basic img (x,y) f@(Filter {fV=boo}))   = Basic img (x,y) f{fV = not boo}
flipNeg :: Basic -> Basic
flipNeg (Basic img (x,y) f@(Filter {neg=boo})) = Basic img (x,y) f{neg = not boo}

-- Convert a Basic picture to an SVG image, represented by a String.
convert :: Basic -> String
convert (Basic (Image (Name name) (width, height)) (x,y) (Filter fH fV neg))
  = "\n  <image x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"" ++ show width ++ "\" height=\"" ++
    show height ++ "\" xlink:href=\"" ++ name ++ "\"" ++ flipPart ++ negPart ++ "/>\n"
        where
          flipPart
            | fH && not fV = " transform=\"translate(0," ++ show (2*y + height) ++ ") scale(1,-1)\" "
            | fV && not fH = " transform=\"translate(" ++ show (2*x + width) ++ ",0) scale(-1,1)\" "
            | fV && fH = " transform=\"translate(" ++ show (2*x + width) ++ "," ++ show (2*y + height) ++ ") scale(-1,-1)\" "
            | otherwise = ""
          negPart
              = if neg
                then " filter=\"url(#negative)\""
                else ""

-- Outputting a picture.
-- The effect of this is to write the SVG code into a file
-- whose path is hardwired into the code. Could easily modify so
-- that it is an argument of the call, and indeed could also call
-- the browser to update on output.
render :: Picture -> IO ()
render pic
 =
   let
       picList = flatten (0,0) pic
       svgString = concatMap convert picList
       newFile = preamble ++ svgString ++ postamble
   in
     do
       outh <- openFile "result.svg" WriteMode
       hPutStrLn outh newFile
       hClose outh

-- Preamble and postamble: boilerplate XML code.

preamble :: [Char]
preamble
 = "<svg width=\"100%\" height=\"100%\" version=\"1.1\"\n" ++
   "xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" ++
   "<filter id=\"negative\">\n" ++
   "<feColorMatrix type=\"matrix\"\n"++
   "values=\"-1 0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  1  1  1  0  0\" />\n" ++
   "</filter>\n"

postamble :: String
postamble
 = "\n</svg>\n"

loadImage :: String -> Int -> Int -> Picture
loadImage i x y= Img $ Image (Name i) (x, y)

-- Example picture
lambda :: Picture
lambda = loadImage "lambda.png" 100 100

-- lambda = Img $ Image (Name "lambda.png") (100, 100)


-- quicksort implementation
-- [5,6,7,1,2,3]
-- [1,2,3] [5] [6,7]
-- [1] [2,3] [5] [6] [7]
-- [1] [2] [3] [5] [6] [7]
-- [1] [2,3] [5] [6] [7]
-- [1,2,3] [5] [6,7]
-- [1,2,3,5,6,7]
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) =
  let soe = filter (<= x) xs
      lrg = filter (> x) xs
  in sort soe ++ [x] ++ sort lrg

-- groups equal values together
-- "Hello" -> ["H","e","ll","o"]
-- [1,2,3,4,2] -> [[1], [2,2], [3], [4]]
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

infinite :: a -> [a]
infinite a = a : infinite a

counter :: [Integer]
counter = counterAcc 1
  where
    counterAcc n = n : counterAcc (n+1)


-- TODO: with the following function we can create an infinite list of
-- something with the infinite function above
-- with that knowledge can you build a infinite list of Pictures, that
-- inverts every second one
-- so that infiniteCheckerList lambda returns
-- [lambda, (invert lambda), lambda, (invert lambda)]

infiniteCheckerList :: Picture -> [Picture]
infiniteCheckerList = undefined

-- TODO: Create a picture called checkerBoard that shows an 8×8 grid of
-- alternating images — one normal, one inverted.
-- following functions can help you
-- infiniteCheckerList (gives you alternating pictures),
-- unpackX (places pictures beside each other),
-- unpackY (stacks rows on top of each other),
-- take (to limit the infinite lists to 8 elements).

checkerBoard :: Picture
checkerBoard = undefined

-- TODO: Create a generic function for the checkerboard, taking a width and a
-- height
xyCheckerP :: Int -> Int -> Picture -> Picture
xyCheckerP x y p = undefined

-- TODO: in Haskell Fibonacci numbers are easy to define
-- Implement a infinite list of the fibonacci numbers
-- every number is the sum of the 2 previous numbers, starting with 0,1
-- 0:1:1:2:3:5:8:13:21:34,55,89...

fib :: [Integer]
fib = undefined

-- TODO: implement a function that can count the occurances of 
-- characters in a string.
-- Usefull functions for this are
-- map (transforms a list of a to a list of b, using a function
-- from a -> b) (a -> b) -> [a] -> [b]
-- sort (takes a list and sorts it) Ord a => [a] -> [a]
-- group (takes a list and groups the values that are equals)
-- head (gives the first element from a list)
-- lenght (gives the length of a list)
countChars :: String -> [(Int, Char)]
countChars = undefined

