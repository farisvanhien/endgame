-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Control.Arrow
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Play


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState {player = pp})
{- 
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0 pp 
  | otherwise 
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs } 
-}
  = -- Just update the elapsed time
    return $ newGS
  where mpp = move pp                  --calculate new position od Player
        resetDirP = setVec vecInit mpp --resets the direction vector
        oldGS = gstate {infoToShow = printPlayer mpp, elapsedTime = elapsedTime gstate + secs, player = mpp}
        newGS = updateEntities oldGS
        updateEntities = movePBullets
        
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    = inputController gstate
    where inputController = (handleMove c) >>> (shooting c)
inputKey (EventKey (Char c) Up _ _) gstate
    = stopMove c gstate
inputKey _ gstate = gstate -- Otherwise keep the same



--move player bullets
movePBullets :: GameState -> GameState
movePBullets gs@(GameState {pBullets = bs})
    = gs {pBullets = moveB bs}

moveB :: [Bullet] -> [Bullet]
moveB [] = []
moveB (x:xs) = (move x) : (moveB xs)






shooting :: Char -> GameState -> GameState
shooting c gs | c == ' '  = gs {pBullets = newBullet : (pBullets gs)}
              | otherwise = gs
              where newBullet = Bullet (pPos p) (pAim p) 10
                    p = player gs
  
stopMove :: Char -> GameState -> GameState
stopMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = Vec (getX c) (getY c)
          oldVec = pDir (player gs)
          getX c   | c == 'd' && (oldX oldVec) > 0 = 0
                   | c == 'a' && (oldX oldVec) < 0 = 0
                   | otherwise = oldX oldVec
          getY c   | c == 'w' && (oldY oldVec) > 0 = 0
                   | c == 's' && (oldY oldVec) < 0 = 0
                   | otherwise = oldY oldVec
          oldX (Vec x _) = x
          oldY (Vec _ y) = y

handleMove :: Char -> GameState -> GameState
handleMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = Vec (getX c) (getY c)
          oldVec = pDir (player gs)
          getX c   | c == 'd'  = moveSpeed
                   | c == 'a'  = (-moveSpeed)
                   | otherwise = oldX oldVec
          getY c   | c == 'w'  = moveSpeed
                   | c == 's'  = (-moveSpeed)
                   | otherwise = oldY oldVec
          oldX (Vec x _) = x
          oldY (Vec _ y) = y          
          
setVec :: Play.Vector -> Player -> Player
setVec v p = p {pDir = v}
          
printPlayer :: Player -> InfoToShow
printPlayer (Player {pPos = pos}) = f1 pos
    where f1 (Pt x y) = ShowACircle x y