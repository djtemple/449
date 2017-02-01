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
  if (c == Black) 
  then print "Enter the move coordinates for player Black in the form 'src x srcy dstx dst y'\n (0 <= n <= 4, or just enter return for a pass) B2: "
  else print "Enter the move coordinates for player White in the form 'src x srcy dstx dst y'\n (0 <= n <= 4, or just enter return for a pass) W2: "
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else return (Just [((coords !! 0), (coords !! 1)), ((coords !! 2), (coords !! 3))])

human b PawnPlacement c = do
  if (c == Black) 
  then print "B"
  else print "W"
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else return (Just [((coords !! 0), (coords !! 1))])


getInts :: String -> [Int]
getInts = map read . words 

checkInput :: [Int] -> Bool
checkInput x | ((length x) < 4) = False
             | (((x !! 0) < 0) || ((x !! 0) > 4) = False
             | (((x !! 1) < 0) || ((x !! 1) > 4) = False
             | (((x !! 2) < 0) || ((x !! 2) > 4) = False
             | (((x !! 0) < 3) || ((x !! 3) > 4) = False

checkMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkMove b player src dst | ((getFromBoard b src) == E) = False
                           | (((getFromBoard b src) == WK) && (player == Black)) = False
                           | (((getFromBoard b src) == WP) && (player == Black)) = False
                           | (((getFromBoard b src) == BK) && (player == White)) = False
                           | (((getFromBoard b src) == BP) && (player == White)) = False
                        | otherwise = True
