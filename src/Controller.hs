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
        updateEntities = movePBullets >>> stayInField >>> deleteOutOfField >>> makeInfoList
        
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    = inputController gstate
    where inputController = (handleMove c) >>> (pewPew c)
inputKey (EventKey (Char c) Up _ _) gstate
    = stopMove c gstate
inputKey (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) gstate 
    =  shoot xPos yPos gstate
inputKey _ gstate = gstate -- Otherwise keep the same

makeInfoList :: GameState -> GameState
makeInfoList gstate = gstate {infoToShow = newList}
        where p1 = player gstate
              bs1 = pBullets gstate
              newList = (printBullets bs1) ++ [printPlayer p1]

--move player bullets
movePBullets :: GameState -> GameState
movePBullets gs@(GameState {pBullets = bs})
    = gs {pBullets = moveB bs}

moveB :: [Bullet] -> [Bullet]
moveB [] = []
moveB (x:xs) = (move x) : (moveB xs)


pewPew :: Char -> GameState -> GameState
pewPew c gs | c == 'p' = shoot 0 200 gs
            | otherwise = gs



stayInField :: GameState -> GameState
stayInField gs = gs {player = setPos newPos (player gs)}
               where pos = pPos (player gs)
                     newPos = ((getPX),(getPY))
                     getPX | getX pos > fieldWidth    = fieldWidth - moveSpeed
                           | getX pos < (-fieldWidth) = (-fieldWidth) + moveSpeed
                           | otherwise = getX pos
                     getPY | getY pos > fieldHeight    = fieldHeight - moveSpeed
                           | getY pos < (-fieldHeight) = (-fieldHeight) + moveSpeed
                           | otherwise = getY pos
                           
deleteOutOfField :: GameState -> GameState                           
deleteOutOfField gs = gs {pBullets = newBL}
                    where newBL = filter p (pBullets gs)
                          p (Bullet {bPos = pos}) = (pointInField pos)
                        
pointInField :: Point -> Bool
pointInField p@(x,y) = (x > (-fieldWidth)) && (x < fieldWidth) && (y > (-fieldHeight)) && (y < fieldHeight)

fieldWidth :: Float 
fieldWidth = 400
fieldHeight :: Float
fieldHeight = 400
  
shoot :: Float -> Float -> GameState -> GameState
shoot xPos yPos gstate = gstate {pBullets = list}
        where 
            normVec = normalizeV newVec
            newVec = (xPos, yPos) - playerPos
            bullet = Bullet playerPos speedVec 10
            list = bullet : (pBullets gstate)
            playerPos = pPos (player gstate)
            speedVec = 2*normVec

stopMove :: Char -> GameState -> GameState
stopMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = ((getPX c),(getPY c))
          oldVec = pDir (player gs)
          getPX c   | c == 'd' && (getX oldVec) > 0 = 0
                    | c == 'a' && (getX oldVec) < 0 = 0
                    | otherwise = getX oldVec
          getPY c   | c == 'w' && (getY oldVec) > 0 = 0
                    | c == 's' && (getY oldVec) < 0 = 0
                    | otherwise = getY oldVec

handleMove :: Char -> GameState -> GameState
handleMove c gs = gs {player = setVec newVec (player gs)}
    where newVec = ((getPX c),(getPY c))
          oldVec = pDir (player gs)
          getPX c   | c == 'd'  = moveSpeed
                    | c == 'a'  = (-moveSpeed)
                    | otherwise = getX oldVec
          getPY c   | c == 'w'  = moveSpeed
                    | c == 's'  = (-moveSpeed)
                    | otherwise = getY oldVec
                    
setVec :: Vector -> Player -> Player
setVec v p = p {pDir = v}

setPos :: Point -> Player -> Player
setPos po p = p {pPos = po}
          
printPlayer :: Player -> InfoToShow
printPlayer (Player {pPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y playerColor playerRadius
    
printBullets :: [Bullet] -> [InfoToShow]
printBullets [] = []
printBullets bullets@(x:xs) = map printBullet bullets

printBullet :: Bullet -> InfoToShow
printBullet (Bullet {bPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y playerBulletColor playerBulletRadius