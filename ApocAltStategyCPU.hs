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

module ApocAltStrategyCPU (
   cpu
   ) where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import System.Random


{- |
-}
cpuAlt :: Chooser
cpuAlt b Normal        player = randomMoveNormal b player
cpuAlt b PawnPlacement player = randomMovePawn (theBoard b) player
