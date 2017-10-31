-- | This module contains the data types
--   which represent the state of the game
module Model where

import Play

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowACircle Float Float

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player      :: Player
                 , pBullets    :: [Bullet]
                 }

initialState :: GameState
initialState = GameState (ShowACircle 0 0) 0 p1 []
             where p1 = Player {pPos = (0, 0), pDir = (0, 0), pAim = (10, 0),pHealth = 100}
             