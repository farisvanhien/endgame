-- | This module contains the data types
--   which represent the state of the game
module Model where

import Play
import Graphics.Gloss
import System.Random

data InfoToShow = ShowNothing
                | ShowANumber Float Float Float Int
                | ShowAString Float Float String
                | ShowAChar   Char
                | ShowACircle Float Float Color Float

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data PlayStatus = Playing
                | Paused
                | GameOver
				| WriteFile
                deriving (Eq, Show)

data GameState = GameState {
                   infoToShow  :: [InfoToShow]
                 , playStatus  :: PlayStatus
                 , elapsedTime :: Float
                 , player      :: Player
                 , pBullets    :: [Bullet]
                 , enemies     :: [Enemy]
                 , eBullets    :: [Bullet]
                 , rNumbers    :: [Int]
                 , score       :: Int
                 }

initialState :: GameState
<<<<<<< HEAD
initialState = GameState i1 Playing 0 p1 [] [] [] g1 0
            where i1 = [ShowACircle 0 0 playerColor playerRadius]
                  p1 = Player {pPos = (0, 0), pDir = (0, 0), pAim = (10, 0),pHealth = 100}
                  g1 = randoms (mkStdGen 42)

						
						
						
						
						
						
												
=======
>>>>>>> cac0dd0bb545ee3a1351612642289e9355e2553d
