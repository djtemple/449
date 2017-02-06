{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3
This module is used for CPSC 449 for the Apocalypse assignment.
Feel free to modify this file as you see fit.
-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
  putStrLn "\nThe initial board:"
  print initBoard
  move initBoard human human

move :: GameState -> Chooser -> Chooser -> IO()
move a b w = do
  bMove <- b a Normal Black
  wMove <- w a Normal White
  let new = update a bMove wMove
  if ((bMove == Nothing) && (wMove == Nothing))then (putStrLn "Done") else putStrLn (show new) 
  if ((bMove == Nothing) && (wMove == Nothing))then (putStrLn "Game over") else move new b w 

update :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> GameState
update a black white = GameState
                        (if (black == Nothing) 
                        then Passed 
                        else if (checkMove a Black ((fromJust black) !! 0) ((fromJust black) !! 1))
                             then Played (((fromJust black) !! 0),((fromJust black) !! 1))
                             else Goofed (((fromJust black) !! 0),((fromJust black) !! 1)))
                       (if (checkMove a Black ((fromJust black) !! 0) ((fromJust black) !! 1))
                        then (blackPen a) 
                        else ((blackPen a) + 1))
                       (if (white == Nothing) 
                        then Passed 
                        else if (checkMove a White ((fromJust white) !! 0) ((fromJust white) !! 1))
                             then Played (((fromJust white) !! 0),((fromJust white) !! 1))
                             else Goofed (((fromJust white) !! 0),((fromJust white) !! 1)))
                        (if (checkMove a White ((fromJust white) !! 0) ((fromJust white) !! 1))
                         then (whitePen a) 
                         else ((whitePen a) + 1))
                        (replace2
                          (replace2
                            (replace2
                              (replace2 
                                (theBoard a) 
                                ((fromJust black) !! 1)
                                (getFromBoard (theBoard a) ((fromJust black) !! 0)))
                              ((fromJust black) !! 0)
                               E)
                            ((fromJust white) !! 1)
                            (getFromBoard (theBoard a) ((fromJust white) !! 0)))
                          ((fromJust white) !! 0)
                          E)

-- | error checking
checkMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkMove b player src dst  | ((getFromBoard (theBoard b) src)== E)                                        = False
                            | (((getFromBoard (theBoard b) src) == WK) && (player == Black))               = False
                            | (((getFromBoard (theBoard b) src) == WP) && (player == Black))               = False
                            | (((getFromBoard (theBoard b) src) == BK) && (player == White))               = False
                            | (((getFromBoard (theBoard b) src) == BP) && (player == White))               = False
                            | (((getFromBoard (theBoard b) src) == WK) && (checkKnightMove b White src dst)) = True
                            | (((getFromBoard (theBoard b) src) == WP) && (checkPawnMove b White src dst))   = True
                            | (((getFromBoard (theBoard b) src) == BK) && (checkKnightMove b Black src dst)) = True
                            | (((getFromBoard (theBoard b) src) == BP) && (checkPawnMove b Black src dst))   = True
                            | otherwise                                                                    = False


checkPawnMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkPawnMove b Black (x,y) (x',y')| ((((y - y') == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WP)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WK)) = True
                                   | otherwise = False

checkPawnMove b White (x,y) (x',y')| ((((y' - y) == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BP)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BK)) = True
                                   | otherwise = False


checkKnightMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkKnightMove b Black (x,y) (x',y')| (getFromBoard (theBoard b) (x',y') == BP) = False
                                     | (getFromBoard (theBoard b) (x',y') == BK) = False
                                     | (((abs (x - x')) == 1) && ((abs (y - y')) == 2)) = True
                                     | (((abs (x - x')) == 2) && ((abs (y - y')) == 1)) = True
                                     | otherwise = False     
checkKnightMove b White (x,y) (x',y')| (getFromBoard (theBoard b) (x',y') == WP) = False
                                     | (getFromBoard (theBoard b) (x',y') == WK) = False
                                     | (((abs (x - x')) == 1) && ((abs (y - y')) == 2)) = True
                                     | (((abs (x - x')) == 2) && ((abs (y - y')) == 1)) = True
                                     | otherwise = False     


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
