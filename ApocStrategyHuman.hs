{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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
This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman (
   human
   ) where

import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human b Normal c = do
  print (prompt Normal c)
  input <- getLine
  let coords = getInts input
  if (((length coords) == 0) || ((checkInput coords) == False))
  then return Nothing
  else return (Just [((coords !! 0), (coords !! 1)), ((coords !! 2), (coords !! 3))])

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human b PawnPlacement c = do
  print (prompt PawnPlacement c)
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else return (Just [((coords !! 0), (coords !! 1))])

{- | Helper function. Creates an integer list from the input string.
-}
getInts :: String -> [Int]
getInts = map read . words 

{- | Checks that a players input move is valid
     Returns True if: the move follows the rules for the piece in the source square
     Returns False if: the source square is empty
                       the source square belongs to the other player
                       any other circumstances
-}
checkMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkMove b player src dst  | ((getFromBoard (theBoard b) src)== E)                                          = False
                            | (((getFromBoard (theBoard b) src) == WK) && (player == Black))                 = False
                            | (((getFromBoard (theBoard b) src) == WP) && (player == Black))                 = False
                            | (((getFromBoard (theBoard b) src) == BK) && (player == White))                 = False
                            | (((getFromBoard (theBoard b) src) == BP) && (player == White))                 = False
                            | (((getFromBoard (theBoard b) src) == WK) && (checkKnightMove b White src dst)) = True
                            | (((getFromBoard (theBoard b) src) == WP) && (checkPawnMove b White src dst))   = True
                            | (((getFromBoard (theBoard b) src) == BK) && (checkKnightMove b Black src dst)) = True
                            | (((getFromBoard (theBoard b) src) == BP) && (checkPawnMove b Black src dst))   = True
                            | otherwise                                                                      = False

{- | Helper function. Checks that a pawn is moving appropriately - forward to an empty cell or diagonally forward to an enemy cell.
-}
checkPawnMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkPawnMove b Black (x,y) (x',y')| ((((x - x') == 1) && (y == y')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((x - x') == 1) && ((abs (y - y')) == 1)) && (getFromBoard (theBoard b) (x',y') == WP)) = True
                                   | ((((x - x') == 1) && ((abs (y - y')) == 1)) && (getFromBoard (theBoard b) (x',y') == WK)) = True
                                   | otherwise = False

checkPawnMove b White (x,y) (x',y')| ((((x' - x) == 1) && (y == y')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((x' - x) == 1) && ((abs (y - y')) == 1)) && (getFromBoard (theBoard b) (x',y') == BP)) = True
                                   | ((((x' - x) == 1) && ((abs (y - y')) == 1)) && (getFromBoard (theBoard b) (x',y') == BK)) = True
                                   | otherwise = False


{- | Helper function. Checks that a knight is moving appropriately - in an 'L' to an empty or enemy cell
-}
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


{- | Helper function. Checks the user input contains all necessary values and all values are in range
-}
checkInput :: [Int] -> Bool
checkInput x | ((length x) < 4) = False
             | (((x !! 0) < 0) || ((x !! 0) > 4)) = False
             | (((x !! 1) < 0) || ((x !! 1) > 4)) = False
             | (((x !! 2) < 0) || ((x !! 2) > 4)) = False
             | (((x !! 3) < 0) || ((x !! 3) > 4)) = False
             | otherwise = True

{- | Helper function. Selects the appropriate prompt based on player and playtype
-}
prompt :: PlayType -> Player -> String
prompt Normal Black        = "Enter the move coordinates for player Black in the form 'srcx srcy dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) B2: "
prompt Normal White        = "Enter the move coordinates for player White in the form 'srcx srcy dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) W2: "
prompt PawnPlacement Black = "Enter the move coordinates for player Black in the form 'dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) B1: "
prompt PawnPlacement White = "Enter the move coordinates for player Black in the form 'dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) W1: "

