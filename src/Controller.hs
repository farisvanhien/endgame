-- | This module defines how the state changes
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Control.Arrow
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
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
        oldGS = gstate {infoToShow = [printPlayer mpp], elapsedTime = elapsedTime gstate + secs, player = mpp}
        newGS = updateEntities oldGS
        updateEntities = movePBullets >>> stayInField >>> makeInfoList
        
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    = inputController gstate
    where inputController = (handleMove c)
inputKey (EventKey (Char c) Up _ _) gstate
    = stopMove c gstate
inputKey (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) gstate = shoot xPos yPos gstate
inputKey _ gstate = gstate -- Otherwise keep the same


makeInfoList :: GameState -> GameState
makeInfoList gstate = gstate {infoToShow = newList}
        where p1 = player gstate
              bs1 = pBullets gstate
              newList = (printPlayer p1) : (printBullets bs1)

--move player bullets
movePBullets :: GameState -> GameState
movePBullets gs@(GameState {pBullets = bs})
    = gs {pBullets = moveB bs}

moveB :: [Bullet] -> [Bullet]
moveB [] = []
moveB (x:xs) = (move x) : (moveB xs)


stayInField :: GameState -> GameState
stayInField gs = gs {player = setPos newPos (player gs)}
               where pos = pPos (player gs)
                     newPos = ((getX),(getY))
                     getX | oldX pos > temp = temp2
                          | oldX pos < (-temp) = (-temp2)
                          | otherwise = oldX pos
                     getY | oldY pos > temp = temp2
                          | oldY pos < (-temp) = (-temp2)
                          | otherwise = oldY pos
                     oldX (x, _) = x
                     oldY (_, y) = y

temp :: Float
temp = 400
temp2 :: Float
temp2 = temp - moveSpeed

{-
shooting :: Char -> GameState -> GameState
shooting c gs | c == ' '  = gs {pBullets = newBullet : (pBullets gs)}
              | otherwise = gs
              where newBullet = Bullet (pPos p) (pAim p) 10
                    p = player gs
  -}
  
shoot :: Float -> Float -> GameState -> GameState
shoot xPos yPos gstate = gstate {pBullets = list}
		where 
			normVec = normalizeV newVec
			newVec = calVec playerPos (xPos, yPos)
			bullet = Bullet playerPos normVec 10
			list = bullet : (pBullets gstate)
			playerPos = pPos (player gstate)
			  


stopMove :: Char -> GameState -> GameState
stopMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = ((getX c),(getY c))
          oldVec = pDir (player gs)
          getX c   | c == 'd' && (oldX oldVec) > 0 = 0
                   | c == 'a' && (oldX oldVec) < 0 = 0
                   | otherwise = oldX oldVec
          getY c   | c == 'w' && (oldY oldVec) > 0 = 0
                   | c == 's' && (oldY oldVec) < 0 = 0
                   | otherwise = oldY oldVec
          oldX (x, _) = x
          oldY (_, y) = y

handleMove :: Char -> GameState -> GameState
handleMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = ((getX c),(getY c))
          oldVec = pDir (player gs)
          getX c   | c == 'd' && (oldX oldVec < 300) = moveSpeed
                   | c == 'a' && (oldX oldVec > (-300)) = (-moveSpeed)
                   | otherwise = oldX oldVec
          getY c   | c == 'w' && (oldY oldVec < 300) = moveSpeed
                   | c == 's' && (oldY oldVec > (-300)) = (-moveSpeed)
                   | otherwise = oldY oldVec
          oldX (x, _) = x
          oldY (_, y) = y
          
setVec :: Vector -> Player -> Player
setVec v p = p {pDir = v}

setPos :: Point -> Player -> Player
setPos po p = p {pPos = po}
          
printPlayer :: Player -> InfoToShow
printPlayer (Player {pPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y
    
printBullets :: [Bullet] -> [InfoToShow]
printBullets bullets = map printBullet bullets

printBullet :: Bullet -> InfoToShow
printBullet (Bullet {bPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y