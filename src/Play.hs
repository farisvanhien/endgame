{-# LANGUAGE FlexibleInstances #-}

module Play where
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Player = Player {
                 pPos    :: Point
               , pDir    :: Vector
               , pAim    :: Vector
               , pHealth :: Int
                     }
             deriving (Show)
                                 
data Enemy = Enemy {
                 ePos    :: Point
               , eDir    :: Vector
               , eHealth :: Int
               , eType   :: EType
                   }
             deriving (Show)
           
data Bullet = Bullet {  
                 bPos    :: Point
               , bDir    :: Vector
               , bDamage :: Int
                     }
            deriving (Show)
                     
data EType = NormalE
           | DamageE
           | HealthE
           deriving (Show)
          
plusVec :: Vector -> Vector -> Vector
plusVec (x1, y1) (x2, y2) = ((x1 + x2),(y1 + y2))
            
moveSpeed :: Float
moveSpeed = 5
vecInit  :: Vector
vecInit  = (0, 0)
playerColor :: Color
playerColor = green
playerRadius :: Float
playerRadius = 10
playerBulletColor :: Color
playerBulletColor = white
playerBulletRadius :: Float
playerBulletRadius = 5
enemyColor :: Color
enemyColor = red
enemyRadius :: Float
enemyRadius = 15
enemyBulletDamage :: Int
enemyBulletDamage = 10

getX :: Point -> Float
getX (x,_) = x
getY :: Point -> Float
getY (_,y) = y
{-
normalizeVector :: Vector -> Vector
normalizeVector (f1, f2) = ((f1/l), (f2/l))
                               where l = sqrt(f1*f1 + f2*f2)-}
                               
movePoint :: Point -> Vector -> Point
movePoint (p1, p2) (v1, v2) = ((p1+v1), (p2+v2))

detectHit :: Point -> Point -> Float -> Bool
detectHit (p1, p2) (p3, p4) r =
               (p1 >= (p3 - r)) && (p1 <= (p3 + r)) && (p2 >= (p4 - r)) && (p2 <= (p4 + r))
               
class Moves a where
    position :: a -> Point
    direction:: a -> Vector
    setPos   :: Point  -> a -> a
    setDir   :: Vector -> a -> a
    move     :: a -> a
   
instance Moves Player where
    position  (Player {pPos = pos}) = pos
    direction (Player {pDir = dir}) = dir
    setPos pos p = p {pPos = pos}
    setDir dir p = p {pDir = dir}
    move p@(Player {pPos = pos, pDir = dir}) = p {pPos = (movePoint pos dir)}
    
instance Moves Enemy where
    position  (Enemy {ePos = pos}) = pos
    direction (Enemy {eDir = dir}) = dir
    setPos pos e = e {ePos = pos}
    setDir dir e = e {eDir = dir}
    move e@(Enemy {ePos = pos, eDir = dir}) = e {ePos = (movePoint pos dir)}
    
instance Moves Bullet where
    position  (Bullet {bPos = pos}) = pos
    direction (Bullet {bDir = dir}) = dir
    setPos pos b = b {bPos = pos}
    setDir dir b = b {bDir = dir}
    move b@(Bullet {bPos = pos, bDir = dir}) = b {bPos = (movePoint pos dir)}
    
class Health a where 
    health :: a -> Int 
    damage :: a -> Int -> a
    isDead :: a -> Bool
    
instance Health Player where 
    health (Player {pHealth = h}) = h
    damage p@(Player {pHealth = h}) x = p {pHealth = (h - x)}
    isDead (Player {pHealth = h}) = h <= 0
    
instance Health Enemy where
    health (Enemy {eHealth = h}) = h
    damage e@(Enemy {eHealth = h}) x = e {eHealth = (h - x)}
    isDead (Enemy {eHealth = h}) = h <= 0
    