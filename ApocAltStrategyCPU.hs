{- |
Module      : ApocAltStrategyCPU
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

module ApocAltStrategyCPU (
   cpuAlt
   ) where

import ApocTools
import ApocStrategyCPU
import System.IO.Unsafe
import System.Random



{- |
-}
cpuAlt :: Chooser
cpuAlt b Normal        player = deterministicMoveNormal b player
cpuAlt b PawnPlacement player = deterministicMovePawn (theBoard b) player

{- | Selectes a random pawn
-}
deterministicMovePawn :: Board -> Player -> IO(Maybe [(Int, Int)])
deterministicMovePawn b player = selectPawnNonRandom ( generateAllEmptyMoves b )


{- | Function that implements the search for a move
-}
deterministicMoveNormal :: GameState -> Player -> IO(Maybe [(Int, Int)])
deterministicMoveNormal b player = chooseNonRandom ( checkMoveGenPlayer (theBoard b) player (generateMove (theBoard b)) )

{- | Selects the first move from a list of moves, but has a 1 in 10 chance of pulling a random move to get out of loops
-}
chooseNonRandom :: [((Int,Int) , (Int, Int))] -> IO (Maybe [(Int,Int)])
chooseNonRandom []   = return(Nothing)
chooseNonRandom list = do
   int <- randomRIO (0 , 10) :: IO Integer
   if int >= 10
       then chooseRandom (list)
       else return (Just (getNormalMoveAtIndex list 0))

{- | Selects a pawn from a list and returns its index
-}
selectPawnNonRandom :: [ (Int,Int)] ->IO (Maybe [(Int,Int)])
selectPawnNonRandom [] = return(Nothing)
selectPawnNonRandom list = do
      return(Just(getPawnMoveAtIndex list 1))

