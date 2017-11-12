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
import View -- try not
import Data.Maybe


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState {player = pp, playStatus = status})
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
  where newGS | status == Playing = updateEntities gstate
              | otherwise         = gstate
        updateEntities = movePlayer >>> stayInField
                         >>> spawnEnemy >>> moveEnemies >>> enemiesInField
                         >>> movePBullets >>> deleteOutOfField 
                         >>> bulletCollision >>> deleteDeadEnemies
                         >>> makeInfoList 
        
        
movePlayer :: GameState -> GameState
movePlayer gstate = gstate {infoToShow = [printPlayer mpp], player = mpp}
                  where mpp = move (player gstate)
        
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    = handleMove c gstate
inputKey (EventKey (Char c) Up _ _) gstate
    = stopMove c gstate
inputKey (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) gstate 
    | (playStatus gstate) == Playing = shoot xPos yPos gstate
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate
    = togglePause gstate
inputKey _ gstate = gstate -- Otherwise keep the same

makeInfoList :: GameState -> GameState
makeInfoList gstate = gstate {infoToShow = newList}
    where p1  = player   gstate
          bs1 = pBullets gstate
          es  = enemies  gstate
          sco = score    gstate
          newList = (printScore sco) : (printBullets bs1) ++ (printEnemies es) ++ [printPlayer p1]
          printScore sco = ShowANumber (-fieldWidth + 10) (fieldHeight - 30) 0.2 sco
              
togglePause :: GameState -> GameState
togglePause gs@(GameState {playStatus = ps})
    | ps == Playing = gs {playStatus = Paused}
    | otherwise     = gs {playStatus = Playing}
              
--move player bullets
movePBullets :: GameState -> GameState
movePBullets gs@(GameState {pBullets = bs})
    = gs {pBullets = moveB bs}

moveB :: [Bullet] -> [Bullet]
moveB [] = []
moveB (x:xs) = (move x) : (moveB xs)


spawnEnemy :: GameState -> GameState
spawnEnemy gs = newGS
              where rands = rNumbers gs
                    r     = head (take 1 rands)
                    updGS = gs {rNumbers = drop 1 rands}
                    digit = mod r 100
                    newGS | (length (enemies gs)) < 10 && (digit == 1) = newEnemy updGS
                          | otherwise  = updGS
                          
newEnemy :: GameState -> GameState
newEnemy gs = gs {enemies = eList, rNumbers = updRs}
            where rands = rNumbers gs
                  rs    = take 4 rands
                  updRs = drop 4 rands
                  guardRs = map encloseInt rs
                  mRs   = map fromIntegral guardRs
                  pos   = ((mRs !! 0),(mRs !! 1))
                  des   = ((mRs !! 2),(mRs !! 3))
                  vec   = des - pos
                  normVec = normalizeV vec
                  newE  = Enemy pos normVec 100 NormalE
                  eList = newE : (enemies gs)

encloseInt :: Int -> Int
encloseInt x = (mod x 800) - 400
                  
moveEnemies :: GameState -> GameState
moveEnemies gs = gs {enemies = newEs}
               where newEs = map move (enemies gs)
               
enemiesInField :: GameState -> GameState
enemiesInField gs@(GameState {enemies = es, rNumbers = rs}) 
    = gs {enemies = newEList, rNumbers = newNumbers}
    where updEnemies = appointEnemy (es,[],rs)
          newEList   = getListE updEnemies
          newNumbers = getListR updEnemies
          getListE (_,e,_) = e 
          getListR (_,_,r) = r

appointEnemy :: ([Enemy], [Enemy], [Int]) -> ([Enemy], [Enemy], [Int])
appointEnemy g@([], es, rands)    = g
appointEnemy ((x:xs), es2, rands) | (not . pointInField) (ePos x)
                                      = appointEnemy (xs, (newE : es2), newRs)
                                  | otherwise 
                                      = appointEnemy (xs, (x : es2), rands)
                                  where rs    = take 2 rands
                                        newRs = drop 2 rands
                                        guardRs = map encloseInt rs
                                        mRs   = map fromIntegral guardRs
                                        pos   = ePos x 
                                        des   = ((mRs !! 0),(mRs !! 1))
                                        vec   = des - pos
                                        normVec = normalizeV vec
                                        newE  = x {eDir = normVec}
                                     
                                     
                                     
                                     
                                     
                                     
bulletCollision :: GameState -> GameState
bulletCollision gs = gs {pBullets = newBullets, enemies = newEs}
    where doCollision = checkBullet ((pBullets gs),[],(enemies gs))
          newBullets  = getB doCollision
          newEs       = getE doCollision
          getB (_,b,_) = b
          getE (_,_,e) = e

checkBullet :: ([Bullet], [Bullet], [Enemy]) -> ([Bullet], [Bullet], [Enemy])
checkBullet g@([], b2, es)   = g
checkBullet ((x:xs), b2, es) = checkBullet (xs, (bul ++ b2), newEs)
                             where checkCol = bulletEnemyCol (Just x, es, [])
                                   bul   = getB checkCol
                                   newEs = getE checkCol
                                   getB (b,_,_) | isNothing b = []
                                                | otherwise   = [fromJust b]
                                   getE (_,_,e) = e

bulletEnemyCol :: (Maybe Bullet, [Enemy], [Enemy]) -> (Maybe Bullet, [Enemy], [Enemy])
bulletEnemyCol g@(_,[],_)   = g        --no more enemies to check
bulletEnemyCol g@(Nothing,_,_)   = g   --if bullet is Nothing (shouldn't happen)
bulletEnemyCol (b,(x:xs),e2) 
    | detectHit (bPos bb) (ePos x) enemyRadius 
        = (Nothing, [], newList)
    | otherwise
        = bulletEnemyCol (b, xs, (x : e2))
    where newList  = xs ++ (newE : e2)
          newE     = damage x (bDamage bb)
          bb       = fromJust b
            
deleteDeadEnemies :: GameState -> GameState
deleteDeadEnemies gs = gs {enemies = newEs, score = newS}
    where oldList = enemies gs
          --newEs = filter (not . isDead) oldList
          newList = delDeadEs (oldList,[],0)
          newEs   = getE newList
          newS    = (getS newList) + (score gs)
          getE (_,e,_) = e 
          getS (_,_,s) = s
          
delDeadEs :: ([Enemy], [Enemy], Int) -> ([Enemy], [Enemy], Int)
delDeadEs g@([],_,_)    = g
delDeadEs ((e:es),e2,i) | isDead e  = delDeadEs (es,e2,(i+10))
                        | otherwise = delDeadEs (es,(e:e2),i)

printEnemies :: [Enemy] -> [InfoToShow]
printEnemies [] = []
printEnemies xx@(x:xs) = map printEnemy xx

printEnemy :: Enemy -> InfoToShow
printEnemy (Enemy {ePos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y enemyColor enemyRadius

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
            newVec = (xPos, yPos) - playerPos
            normVec = normalizeV newVec
            speedVec = 10*normVec
            bullet = Bullet playerPos speedVec 50
            list = bullet : (pBullets gstate)
            playerPos = pPos (player gstate)

stopMove :: Char -> GameState -> GameState
stopMove c gs = gs {player = setDir newVec (player gs)}
    where newVec = ((getPX c),(getPY c))
          oldVec = pDir (player gs)
          getPX c   | c == 'd' && (getX oldVec) > 0 = 0
                    | c == 'a' && (getX oldVec) < 0 = 0
                    | otherwise = getX oldVec
          getPY c   | c == 'w' && (getY oldVec) > 0 = 0
                    | c == 's' && (getY oldVec) < 0 = 0
                    | otherwise = getY oldVec

handleMove :: Char -> GameState -> GameState
handleMove c gs = gs {player = setDir newVec (player gs)}
    where newVec = ((getPX c),(getPY c))
          oldVec = pDir (player gs)
          getPX c   | c == 'd'  = moveSpeed
                    | c == 'a'  = (-moveSpeed)
                    | otherwise = getX oldVec
          getPY c   | c == 'w'  = moveSpeed
                    | c == 's'  = (-moveSpeed)
                    | otherwise = getY oldVec
          
printPlayer :: Player -> InfoToShow
printPlayer (Player {pPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y playerColor playerRadius
    
printBullets :: [Bullet] -> [InfoToShow]
printBullets [] = []
printBullets bullets@(x:xs) = map printBullet bullets

printBullet :: Bullet -> InfoToShow
printBullet (Bullet {bPos = pos}) = f1 pos
    where f1 (x, y) = ShowACircle x y playerBulletColor playerBulletRadius