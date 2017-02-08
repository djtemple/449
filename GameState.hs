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
Parameters:   GameState - current game state
              (x,y) coordinates for human piece move location
              (x,y) coordinates for CPU move move location
-}

update :: GameState -> (Int, Int) -> (Int, Int) -> GameState
