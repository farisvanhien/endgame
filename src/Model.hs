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
                 }

initialState :: GameState
initialState = GameState (ShowACircle 0 0) 0