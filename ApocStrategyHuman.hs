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
Human Strategy Module. Allows for human input on game moves. Written by Rachel Mclean.
-}

module ApocStrategyHuman (
   human
   ) where

import ApocTools

{- | Prompts the user for input for a move
     1. Returns Nothing if the input is empty (Pass)
     2. Returns first four (or two, for pawn placement) numbers of input, if all are in range
     3. Reprompts if the list of numbers is too short or numbers are of range
      --- Rachel Mclean
-}
human    :: Chooser
human b Normal c = do
  putStrLn (prompt Normal c)
  input <- getLine
  putStrLn input
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else if (checkInput coords)
       then return (Just [((coords !! 0), (coords !! 1)), ((coords !! 2), (coords !! 3))])
       else human b Normal c

human b PawnPlacement c = do
  putStrLn (prompt PawnPlacement c)
  input <- getLine
  let coords = getInts input
  if ((length coords) == 0)
  then return Nothing
  else if (checkInput2 coords)
       then return (Just [((coords !! 0), (coords !! 1))])
       else human b PawnPlacement c


{- | Helper function to take the digits from a string input and form an array
      --- Rachel Mclean
-}
getInts :: String -> [Int]
getInts = map read . words 

{- | Helper function to check input for a normal move
     1. Checks that the list of numbers taken from input is at least four numbers long
     2. Checks that each of the four numbers are in range
      --- Rachel Mclean
-}
checkInput :: [Int] -> Bool
checkInput x | ((length x) < 4) = False
             | (((x !! 0) < 0) || ((x !! 0) > 4)) = False
             | (((x !! 1) < 0) || ((x !! 1) > 4)) = False
             | (((x !! 2) < 0) || ((x !! 2) > 4)) = False
             | (((x !! 3) < 0) || ((x !! 3) > 4)) = False
             | otherwise = True

{- | Helper function to check input for a pawn placement move
     1. Checks that the list of numbers taken from input is at least two numbers long
     2. Checks that each of the four numbers are in range
      --- Rachel Mclean
-}
checkInput2 :: [Int] -> Bool
checkInput2 x | ((length x) < 2) = False
              | (((x !! 0) < 0) || ((x !! 0) > 4)) = False
              | (((x !! 1) < 0) || ((x !! 1) > 4)) = False
              | otherwise = True


{- | Helper function to find the appropriate prompt
     1. Checks if the move is normal or pawn placement
     2. Checks if the move is Black or White
      --- Rachel Mclean
-}
prompt :: PlayType -> Player -> String
prompt Normal Black = "Enter the move coordinates for player Black in the form 'srcx srcy dstx dsty' \n(0 <= n <= 4, or just enter return for a pass) B2: "
prompt Normal White = "Enter the move coordinates for player White in the form 'srcx srcy dstx dsty' \n(0 <= n <= 4, or just enter return for a pass) W2: "
prompt PawnPlacement Black = "Enter the move coordinates for player Black in the form 'dstx dsty'\n(0 <= n <= 4, or just enter return for a pass) B1: "
prompt PawnPlacement White= "Enter the move coordinates for player White in the form 'dstx dsty'\n(0 <= n <= 4, or just enter return for a pass) W1: "
                          

