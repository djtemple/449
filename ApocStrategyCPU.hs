{- |
Module      : ApocStrategyCPU
Description : Template  a game-playing strategy definition.
`Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
`License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation  any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software  any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3
This module is used  CPSC 449  the Apocalypse assignment.
This is merely a skeleton to get you started on creating a strategy  playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyCPU (
   cpu
   ) where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random


{- |
-}
cpu :: Chooser
cpu b Normal        player = randomMoveNormal b player
cpu b PawnPlacement player = randomMovePawn (theBoard b) player

{- | Selectes a random pawn
-}
randomMovePawn :: Board -> Player -> IO(Maybe [(Int, Int)])
randomMovePawn b player = selectPawnRandom ( generateAllEmptyMoves b )

{- | Selects a pawn from a list and returns its index
-}
selectPawnRandom :: [ (Int,Int)] ->IO (Maybe [(Int,Int)])
selectPawnRandom [] = return(Nothing)
selectPawnRandom list = do
      int <- randomRIO (0, (length list - 1))
      return(Just(getPawnMoveAtIndex list int))

{- | Retuns the pawn corresponding to a a move
-}
getPawnMoveAtIndex   :: [(Int, Int)] -> Int -> [(Int, Int)]
getPawnMoveAtIndex [] _ = []
getPawnMoveAtIndex [(x,y)] _ = [(x,y)]
getPawnMoveAtIndex ((x,y): list) 0 = [(x,y)]
getPawnMoveAtIndex ((x,y): list) index = getPawnMoveAtIndex list (index - 1)

{- | Function that implements the search for a move
-}
randomMoveNormal :: GameState -> Player -> IO(Maybe [(Int, Int)])
randomMoveNormal b player = chooseRandom ( checkMoveGenPlayer (theBoard b) player (generateMove (theBoard b)) )


{- | Selects a random move frmo a list of moves
-}
chooseRandom :: [((Int,Int) , (Int, Int))] -> IO (Maybe [(Int,Int)])
chooseRandom []   = return(Nothing)
chooseRandom list = do
       int <- randomRIO (0 , (length list -1))
       return (Just (getNormalMoveAtIndex list int))

{- | Returns the move at an index from within a list of moves
-}
getNormalMoveAtIndex :: [((Int , Int), (Int, Int))] -> Int -> [(Int, Int)]
getNormalMoveAtIndex [] _ = []
getNormalMoveAtIndex [(source, dest)] _ = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) 0 = [(source), (dest)]
getNormalMoveAtIndex ((source, dest) : list) x = getNormalMoveAtIndex list (x - 1)

{- | Creates a list of all valid moves
-}
generateMove :: Board -> [ ((Int, Int), (Int, Int))]
generateMove b = generateMoveString (formatString (board2Str b)) 0

{- | Generates a list of valid moves based on the board as a string
-}
generateMoveString :: String -> Int  -> [((Int, Int) , (Int, Int))]
generateMoveString [] _ = []
generateMoveString (c: cs) num |  (num) > 25 || (num) < 0 = []
generateMoveString (c: cs) num = generateMoveFromChar c ( mod num 5, div num 5) ++ generateMoveString cs (num + 1)


{- | A helper function for generateMoveString
-}
generateMoveFromChar :: Char -> (Int, Int) -> [ ((Int, Int), (Int, Int))]
generateMoveFromChar c (x,y) | c == '_' = []
generateMoveFromChar c (x,y) | c == '/' || c == '+' = [((x,y), (x  , y+1))]++ [((x,y), (x +1 , y +1))] ++ [((x,y), (x+1, y))] ++ [((x,y) , (x -1, y +1))] ++
                                                          [((x,y), (x  , y-1))]++ [((x,y), (x -1 , y -1))] ++ [((x,y), (x-1,y ))] ++ [((x,y) , (x +1, y -1))]
generateMoveFromChar c (x,y) | c == 'X' || c == '#' = [ ((x,y) , (x - 2, y + 1))] ++ [((x,y) , (x-1, y+2))] ++ [((x,y) , (x+1, y+2))] ++ [((x, y) , (x+2, y+1))] ++
                                                                      [ ((x,y) , (x + 2, y - 1))] ++ [((x,y) , (x+1, y-2))] ++ [((x,y) , (x-1, y-2))] ++ [((x, y) , (x-2, y-1))]


{- | Checks if a move is valid for a specific player and board
-}
checkMoveGenPlayer :: Board -> Player -> [ ((Int, Int), (Int, Int))] -> [ ((Int, Int) , (Int , Int))]
checkMoveGenPlayer theBoard player [] = []
checkMoveGenPlayer theBoard player ((source, dest) :xs) = if isValidPlayer theBoard player ([(source)] ++ [(dest)])
                               then [(source, dest)] ++ checkMoveGenPlayer theBoard player xs
                               else checkMoveGenPlayer theBoard player xs

{- | Checks if a piece is owned by that player
-}
isValidPlayer :: Board -> Player -> [(Int,Int)] -> Bool
isValidPlayer board player [] = False
isValidPlayer board player (first:rest) | (getFromBoard board first) == E = False
                                        | (player == playerOf (pieceOf(getFromBoard board first)))  = isValidMove board first (head rest)
                                        | True = False

{- | Further checks a valid move based off of the piece and relative locations
-}
isValidMove :: Board -> (Int,Int) -> (Int,Int) -> Bool
isValidMove theBoard (x,y) (w,z)
    | (x<0) || (x>4)|| (y<0) || (y>4) || (w<0) || (w>4) || (z<0) || (z>4) = False
    |((getFromBoard theBoard (x,y)) == WK ) =
        if (((abs (x-w))<3) && ((abs (y-z)))<3)
           && (((abs (x-w)) + (abs (y-z))) == 3)
           && (((getFromBoard theBoard (w,z)) ==  E)
               || ((getFromBoard theBoard (w,z)) ==  BK)
               || ((getFromBoard theBoard (w,z)) ==  BP))
        then True
        else False

    |((getFromBoard theBoard (x,y)) == BK ) =
        if (((abs (x-w))<3) && ((abs (y-z)))<3)
           && (((abs (x-w)) + (abs (y-z))) == 3)
           && (((getFromBoard theBoard (w,z)) ==  E)
               || ((getFromBoard theBoard (w,z)) ==  WK)
               || ((getFromBoard theBoard (w,z)) ==  WP))
        then True
        else False
    |((getFromBoard theBoard (x,y)) == WP ) && (((abs (x-w)) == 0) && ((y-z) == -1) && getFromBoard theBoard (w,z) == E)
        = True
    |((getFromBoard theBoard (x,y)) == WP ) && (((abs (x-w)) == 1) && ((y-z) == -1)) && (((getFromBoard theBoard (w,z)) ==  BK) || ((getFromBoard theBoard (w,z)) ==  BP))
             = True
    |((getFromBoard theBoard (x,y)) == BP ) && (((abs (x-w)) == 0) && ((y-z) == 1) && getFromBoard theBoard (w,z) == E)
        = True
    |((getFromBoard theBoard (x,y)) == BP ) && (((abs (x-w)) == 1) && ((y-z) == 1)) && (((getFromBoard theBoard (w,z)) ==  WK) || ((getFromBoard theBoard (w,z)) ==  WP))
             = True
    |True = False

{- | Takes board as a string and returns the relevant substrings as a string
-}
formatString :: String -> String
formatString s = filter helperformatString (removeFront s)

{- | Helper function for formatString
-}
removeFront :: String -> String
removeFront (x:xs) | x == '\n'  && length xs  >= 0= [] ++ xs
removeFront (x:xs) = removeFront xs

{- | Helper function for formatString
-}
helperformatString :: Char -> Bool
helperformatString '|' = False
helperformatString '\n' = False
helperformatString  c = True


{- | Returns all the moves that end in a currently empty position
-}
generateAllEmptyMoves :: Board -> [(Int, Int)]
generateAllEmptyMoves b = generateAllEmptyMoves' (formatString (board2Str b)) 0

{- | Helper function for generateAllEmptyMoves
-}
generateAllEmptyMoves' :: String -> Int -> [(Int,Int)]
generateAllEmptyMoves' [] index = []
generateAllEmptyMoves' _ index | index > 25 || index < 0 = []
generateAllEmptyMoves' (x:xs) index = if x == '_'
                    then [(mod index 5, div index 5)] ++ generateAllEmptyMoves' xs (index +1)
                    else generateAllEmptyMoves' xs (index + 1)
