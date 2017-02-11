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
      -- * Turn / Update Functions
      move, update, makeBoard,
      -- * Error and Validity Checking
      checkMove, checkKnightMove, checkPawnMove,
      -- * End State Functions
      checkEnd, numPieces, numPieces', winner, wrapUp,
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import System.Exit
import CmdLineArgs
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
main' args = 
	if (args == []) then do
		putStrLn "Welcome to Apocalypse interactive mode, to begin select two player strategies."		
		putStrLn "Names of possible strategies:\n  random\n  deterministic\n  human"				
		putStrLn "Enter name for Black strategy:"
		s1 <- getLine												
		bStrat <- (checkStrat s1)										
		putStrLn "Enter name for White strategy:"
		s2 <- getLine
		wStrat<- (checkStrat s2)
		putStrLn "\nThe initial board:"
		print initBoard
		move initBoard bStrat wStrat								
	--Command line--------------------------
	else
		if (not (( "random" `elem` args && "deterministic" `elem` args) || ("random" `elem` args && "human" `elem` args) || ("deterministic" `elem` args && "human"`elem` args) || ( args == ["random" , "random"]) || (args == ["deterministic" , "deterministic"]) || (args == ["human","human"])) ) then do
			putStrLn "Illegal strategy name entered\nLegal strategy names:\n  random\n  deterministic\n  human\nQuitting Game..."		
			exitSuccess
    else do								
			putStrLn "\nThe initial board:"						
			print initBoard
			bStrat <- checkStrat(args !!0)						
			wStrat <- checkStrat(args !!1)
			move initBoard bStrat wStrat
	
-- | Checks user input for strategy, if valid returns strategy -----------------------
checkStrat :: String -> IO Chooser
checkStrat n
	| (n == "random") = return cpu								
	| (n == "deterministic") = return cpuAlt
	| (n == "human") = return human
	| otherwise = do putStrLn "Illegal strategy name entered\nLegal strategy names:\n  random\n  deterministic\n  human\nQuitting Game..."; exitSuccess

--- Call turns and update game board-----------------------------------------
{- | Calls an ordinary turn: 
     1. prompts each strategy for a move;
     2. checks validity;
     3. updates the game state;
     4. displays updated game state to console
     5. calls next move with updated state
-}
move :: GameState -> Chooser -> Chooser -> IO()
move a b w = do
  bMove <- b a Normal Black
  wMove <- w a Normal White
  let bValid = if (bMove == Nothing) then True else (checkMove a Black ((fromJust bMove) !! 0) ((fromJust bMove) !! 1)) 
  let wValid = if (wMove == Nothing) then True else (checkMove a White ((fromJust wMove) !! 0) ((fromJust wMove) !! 1))
  let new = update a bMove wMove bValid wValid
  putStrLn (show new)
  if (checkEnd new bMove wMove) then (wrapUp new) else move new b w 

{- | updates the gamestate
     1. shows Black move
     2. updates Black penalty
     3. shows White move
     4. updates White penalty
     5. updates the gameboard
-}
update :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Bool -> Bool -> GameState

<<<<<<< Updated upstream
-- | Both players pass
update a Nothing Nothing bValid wValid = GameState
=======
<<<<<<< HEAD
{-|
    The update function takes in the GameState and the moves made to update the
    game board

    GameState - the current game state
    Maybe [(Int, Int)] - the first pawns move coordinates
    Maybe [(Int, Int)] - the second pawns move coordinates

    Returns the Updated GameState

    There is 4 possible outcomes from the input
    - Both players pass
    - Black passes and White moves
    - Black moves and White passes
    - Both players move
-}
update :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> GameState
update a Nothing Nothing = GameState
=======
-- | Both players pass
update a Nothing Nothing bValid wValid = GameState
>>>>>>> origin/master
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
=======
<<<<<<< HEAD
-- Check if pawns are --------------------------------------------------------------------------------------
-- | Checks the board if any panws have reached the opposite end of the board to transform into Knights
checkPawnUpgrades :: GameState -> GameState
checkPawnUpgrades b | (getFromBoard (theBoard))
=======
>>>>>>> Stashed changes
{- | updates the gameboard
     1. simple update if both moves are nothing, or only one player moves
     2. otherwise, checks if pieces are swapping spots and switches them, rather than adding and erasing pieces
     3. checks if pieces are landing on the same square and places a piece, or leaves the cell empty accordingly
     4. otherwise, simply places both pieces
-}
makeBoard :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> [[Cell]]
<<<<<<< Updated upstream
=======
>>>>>>> origin/master
>>>>>>> Stashed changes

-- | no updates (called if both player moves are invalid)
makeBoard a Nothing Nothing = theBoard a

-- | White moves; Black move is Nothing or invalid
makeBoard a Nothing (Just [src, dst]) = 
                        (replace2
                          (replace2 
                            (theBoard a) 
                            dst
                            (getFromBoard (theBoard a) (src)))
                          src
                          E)

-- | Black moves; White move is Nothing or invalid
makeBoard a (Just [src, dst]) Nothing = 
                        (replace2
                          (replace2 
                            (theBoard a) 
                            dst
                            (getFromBoard (theBoard a) (src)))
                          src
                          E)

-- | Both players move. Check if pieces are swapping or clashing and represent accordinly
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
                                          | (b' == w)                = (replace2
                                                                         (replace2
                                                                           (replace2
                                                                             (replace2 
                                                                               (theBoard a) 
                                                                                w'
                                                                               (getFromBoard (theBoard a) w))
                                                                              w
                                                                              E)
                                                                            b'
                                                                           (getFromBoard (theBoard a) b))
                                                                          b
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

-- Error/Validity Checking --------------------------------------------------------------------------------------
{- | Check if an arbitrary move is valid
     1. check that the starting square is not empty
     2. check that the chosen piece belongs to the right player
     3. check that the destination square is in line with the rules of the piece
-}
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

{- | checks if a move is valid for a pawn piece
     1. the pawn is moving one square straight forward into an empty cell
     2. the pawn is moving one square diagonally forward into an enemy cell
-}
checkPawnMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool

-- | Checks for a black pawn (y index should decrease)
checkPawnMove b Black (x,y) (x',y')| ((((y - y') == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WP)) = True
                                   | ((((y - y') == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == WK)) = True
                                   | otherwise = False

-- | Checks for a white pawn (Y index should increase)
checkPawnMove b White (x,y) (x',y')| ((((y' - y) == 1) && (x == x')) && (getFromBoard (theBoard b) (x',y') == E)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BP)) = True
                                   | ((((y' - y) == 1) && ((abs (x - x')) == 1)) && (getFromBoard (theBoard b) (x',y') == BK)) = True
                                   | otherwise = False

{- | checks if a move is valid for a knight piece
     1. the pawn is moving in an L into an empty or enemy cell
-}
checkKnightMove :: GameState -> Player -> (Int, Int) -> (Int, Int) -> Bool

-- | Checks for black knight
checkKnightMove b Black (x,y) (x',y')| (getFromBoard (theBoard b) (x',y') == BP) = False
                                     | (getFromBoard (theBoard b) (x',y') == BK) = False
                                     | (((abs (x - x')) == 1) && ((abs (y - y')) == 2)) = True
                                     | (((abs (x - x')) == 2) && ((abs (y - y')) == 1)) = True
                                     | otherwise = False  

-- | Checks for white knight   
checkKnightMove b White (x,y) (x',y')| (getFromBoard (theBoard b) (x',y') == WP) = False
                                     | (getFromBoard (theBoard b) (x',y') == WK) = False
                                     | (((abs (x - x')) == 1) && ((abs (y - y')) == 2)) = True
                                     | (((abs (x - x')) == 2) && ((abs (y - y')) == 1)) = True
                                     | otherwise = False     

--- End State -------------------------------------------------------------------

{- | Checks if the game is over: 
     1. checks if either player has reached 2 penalties
     2. checks if both players passed
     3. checks if either player has run out of pawns
-}
checkEnd :: GameState -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Bool
checkEnd a b w | ((blackPen a) == 2) = True
               | ((whitePen a) == 2) = True
               | ((b == Nothing) && (w == Nothing)) = True
               | ((numPieces a WP) == 0) = True
               | ((numPieces a BP) == 0) = True
               | otherwise = False

{- | Returns the number of a given piece remaining on the board
-}
numPieces :: GameState -> Cell -> Int
numPieces a p = numPieces' a p 0 0 0

-- | Helper for numPieces
numPieces' :: GameState -> Cell -> Int -> Int -> Int -> Int
numPieces' a p 4 4 sum = (if ((getFromBoard (theBoard a) (4, 4)) == p) then (sum + 1) else sum)
numPieces' a p 4 y sum = numPieces' a p 0 (y + 1) (if ((getFromBoard (theBoard a) (4, y)) == p) then (sum + 1) else sum)
numPieces' a p x y sum = numPieces' a p (x+1) y (if ((getFromBoard (theBoard a) (x, y)) == p) then (sum + 1) else sum)

{- | Checks which player is the winner:
     1. if both players have 2 penalties, choose the player with more pawns
     2. if neither player has 2 penalties, choose the player with more pawns
     3. if one player has 2 penalties and the other does not, choose the player with fewer penalties
     4. if both players have 2 penalties (or neither has 2 penalties) and both players have the same number of pawns, the game is tied
-}
winner :: GameState -> String
winner a | ((((blackPen a) == 2) && ((whitePen a) == 2)) && ((numPieces a WP) > (numPieces a BP))) = "White wins!"
         | (((blackPen a) == 2) && ((whitePen a) /= 2))= "White wins!"
         | ((((blackPen a) /= 2) && ((whitePen a) /= 2)) && ((numPieces a WP) > (numPieces a BP))) = "White wins!"
         | ((((blackPen a) == 2) && ((whitePen a) == 2)) && ((numPieces a WP) == (numPieces a BP))) = "Tie!"
         | ((((blackPen a) /= 2) && ((whitePen a) /= 2)) && ((numPieces a WP) == (numPieces a BP))) = "Tie!"
         | otherwise = "Black wins!"

{- | Ends the game and outputs end state information
     1. Checks the prints the winner
     2. Checks and prints the number of black pawns remaining
     3. Checks and prints the number of white pawns remaining
-}
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
