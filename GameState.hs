{- |
Module      : GameState
Description : Functions that deal with the updating and maintainting of the  game
state.
-}

import ApocTools

{- | Game state code
-}


{- |
Function to get locate and move given pieces to move based on given coordinates
Parameters: (x,y) coordinates for human piece && (x,y) coordinates for CPU move

-}

update :: GameState (Board) -> ((Int, Int), (Int, Int)) -> Board
