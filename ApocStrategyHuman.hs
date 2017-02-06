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


{- | Prompts the user for a move, checks input, and reprompts if input is invalid (out of range or insufficent length)
-}
human    :: Chooser
human b Normal c = do
  putStrLn (prompt Normal c)
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else if (checkInput coords) 
       then return (Just [((coords !! 0), (coords !! 1)), ((coords !! 2), (coords !! 3))])
       else human b Normal c


{- | TPrompts the user for a move, checks input, and reprompts if input is invalid (out of range or insufficent length)
-}
human b PawnPlacement c = do
  putStrLn (prompt PawnPlacement c)
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else if (checkInput2 coords)
       then return (Just [((coords !! 0), (coords !! 1))])
       else human b PawnPlacement c


{- | Helper function. Creates an integer list from the input string.
-}
getInts :: String -> [Int]
getInts = map read . words


{- | Helper function for normal moves. Checks the user input contains all necessary values and all values are in range
-}
checkInput :: [Int] -> Bool
checkInput x | ((length x) < 4) = False
             | (((x !! 0) < 0) || ((x !! 0) > 4)) = False
             | (((x !! 1) < 0) || ((x !! 1) > 4)) = False
             | (((x !! 2) < 0) || ((x !! 2) > 4)) = False
             | (((x !! 3) < 0) || ((x !! 3) > 4)) = False
             | otherwise = True


{- | Helper function for pawn placement. Checks the user input contains all necessary values and all values are in range
-}
checkInput2 :: [Int] -> Bool
checkInput2 x | ((length x) < 2) = False
              | (((x !! 0) < 0) || ((x !! 0) > 4)) = False
              | (((x !! 1) < 0) || ((x !! 1) > 4)) = False
              | otherwise = True

{- | Helper function. Selects the appropriate prompt based on player and playtype
-}
prompt :: PlayType -> Player -> String
prompt Normal Black        = "Enter the move coordinates for player Black in the form 'srcx srcy dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) B2: "
prompt Normal White        = "Enter the move coordinates for player White in the form 'srcx srcy dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) W2: "
prompt PawnPlacement Black = "Enter the move coordinates for player Black in the form 'dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) B1: "
prompt PawnPlacement White = "Enter the move coordinates for player Black in the form 'dstx dsty'\n (0 <= n <= 4, or just enter return for a pass) W1: "

