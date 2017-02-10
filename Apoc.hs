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
import ApocStrategyCPU
import ApocAltStrategyCPU


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

-- Call turns and update game board-----------------------------------------

move :: GameState -> Chooser -> Chooser -> IO()
move a b w = do
  bMove <- b a Normal Black
  wMove <- w a Normal White
  let bValid = if (bMove == Nothing) then True else (checkMove a Black ((fromJust bMove) !! 0) ((fromJust bMove) !! 1)) 
  let wValid = if (wMove == Nothing) then True else (checkMove a White ((fromJust wMove) !! 0) ((fromJust wMove) !! 1))
  let new = update a bMove wMove bValid wValid
  putStrLn (show new)
  if (checkEnd new bMove wMove) then (wrapUp new) else move new b w 

-- | Both players pass
update :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Bool -> Bool -> GameState
update a Nothing Nothing bValid wValid = GameState
                            (Passed)
                            (blackPen a)
                            (Passed)
                            (whitePen a)
                            (theBoard a)

-- | Black moves; White passes
update a black Nothing bValid wValid = GameState
                        (if (bValid)
                         then Played (((fromJust black) !! 0),((fromJust black) !! 1))
                         else Goofed (((fromJust black) !! 0),((fromJust black) !! 1)))
                        (if bValid
                         then (blackPen a) 
                         else ((blackPen a) + 1))
                        (Passed)
                        (whitePen a)
                        (if bValid then (makeBoard a black Nothing) else (theBoard a))
                    
-- | White moves; Black passes
update a Nothing white bValid wValid = GameState
                        (Passed)
                        (blackPen a)
                        (if wValid
                         then Played (((fromJust white) !! 0),((fromJust white) !! 1))
                         else Goofed (((fromJust white) !! 0),((fromJust white) !! 1)))
                        (if wValid
                         then (whitePen a) 
                         else ((whitePen a) + 1))
                        (if wValid then (makeBoard a Nothing white) else (theBoard a))
                        
-- | Both players move
update a black white bValid wValid = GameState
                        (if bValid
                         then Played (((fromJust black) !! 0),((fromJust black) !! 1))
                         else Goofed (((fromJust black) !! 0),((fromJust black) !! 1)))
                        (if bValid
                         then (blackPen a) 
                         else ((blackPen a) + 1))
                        (if wValid
                         then Played (((fromJust white) !! 0),((fromJust white) !! 1))
                         else Goofed (((fromJust white) !! 0),((fromJust white) !! 1)))
                        (if wValid
                         then (whitePen a) 
                         else ((whitePen a) + 1))
                        (makeBoard a (if bValid then black else Nothing) (if wValid then white else Nothing))

makeBoard :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> [[Cell]]
makeBoard a Nothing Nothing = theBoard a
makeBoard a Nothing (Just [src, dst]) = 
                        (replace2
                          (replace2 
                            (theBoard a) 
                            dst
                            (getFromBoard (theBoard a) (src)))
                          src
                          E)

makeBoard a (Just [src, dst]) Nothing = 
                        (replace2
                          (replace2 
                            (theBoard a) 
                            dst
                            (getFromBoard (theBoard a) (src)))
                          src
                          E)

makeBoard a (Just [b, b']) (Just [w, w']) | ((b == w') && (w == b')) = (replace2
                                                                         (replace2 
                                                                           (theBoard a) 
                                                                            b'
                                                                           (getFromBoard (theBoard a) (b)))
                                                                          w'
                                                                         (getFromBoard (theBoard a) w))
                                          | (b' == w')               = (replace2
                                                                         (replace2
                                                                            (replace2 
                                                                               (theBoard a) 
                                                                                b'
                                                                               (if (((getFromBoard (theBoard a) b) == BK) && ((getFromBoard (theBoard a) w) == WP)) then BK else
                                                                                 (if (((getFromBoard (theBoard a) b) == BP) && ((getFromBoard (theBoard a) w) == WK)) then WK else E)))
                                                                             b
                                                                             E)
                                                                           w
                                                                           E)
                                          | otherwise                = (replace2
                                                                         (replace2
                                                                           (replace2
                                                                             (replace2 
                                                                               (theBoard a) 
                                                                                b'
                                                                               (getFromBoard (theBoard a) b))
                                                                              b
                                                                              E)
                                                                            w'
                                                                           (getFromBoard (theBoard a) w))
                                                                          w
                                                                          E)

-- Check if pawns are --------------------------------------------------------------------------------------
-- | Checks the board if any panws have reached the opposite end of the board to transform into Knights
checkPawnUpgrades :: GameState -> GameState
checkPawnUpgrades b | (getFromBoard (theBoard)) 






-- Error/Validity Checking --------------------------------------------------------------------------------------
-- | Checks if an arbitrary move is valid
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

-- | Checks if a pawn move is valid
checkPawnMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkPawnMove b Black (x,y) (x',y')| ((((y - y') == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WP)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WK)) = True
                                   | otherwise = False

checkPawnMove b White (x,y) (x',y')| ((((y' - y) == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BP)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BK)) = True
                                   | otherwise = False

-- | Checks if a knight move is valid
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

--- End State -------------------------------------------------------------------
-- | checks if the game is over
checkEnd :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Bool
checkEnd a b w | ((blackPen a) == 2) = True
               | ((whitePen a) == 2) = True
               | ((b == Nothing) && (w == Nothing)) = True
               | ((numPieces a WP) == 0) = True
               | ((numPieces a BP) == 0) = True
               | otherwise = False

-- | returns the number of pawns remaining on the board for a given player
numPieces :: GameState -> Cell -> Int
numPieces a p = numPieces' a p 0 0 0

-- | Helper for numPieces
numPieces' :: GameState -> Cell -> Int -> Int -> Int -> Int
numPieces' a p 4 4 sum = sum
numPieces' a p 4 y sum = numPieces' a p 0 (y + 1) (if ((getFromBoard (theBoard a) (4, y)) == p) then (sum + 1) else sum)
numPieces' a p x y sum = numPieces' a p (x+1) y (if ((getFromBoard (theBoard a) (x, y)) == p) then (sum + 1) else sum)

-- | Determines whether Black is the winner, White is the winner, or the game ends in a tie
winner :: GameState -> String
winner a | ((((blackPen a) == 2) && ((whitePen a) == 2)) && ((numPieces a WP) > (numPieces a BP))) = "White wins!"
         | ((((blackPen a) /= 2) && ((whitePen a) /= 2)) && ((numPieces a WP) > (numPieces a BP))) = "White wins!"
         | (((blackPen a) == 2) && ((whitePen a) /= 2))= "White wins!"
         | ((((blackPen a) == 2) && ((whitePen a) == 2)) && ((numPieces a WP) == (numPieces a BP))) = "Tie!"
         | ((((blackPen a) /= 2) && ((whitePen a) /= 2)) && ((numPieces a WP) == (numPieces a BP))) = "Tie!"
         | otherwise = "Black wins!"

-- | Prints end of game info to console
wrapUp :: GameState -> IO()
wrapUp a = do
  putStr (winner a)
  putStr " Black: "
  putStr (show (numPieces a BP))
  putStr "  White: "
  putStr (show (numPieces a WP))

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
