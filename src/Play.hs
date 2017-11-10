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
           
data Bullet = Bullet {  
                 bPos   :: Point
               , bDir   :: Vector
               , damage :: Int
                     }
            deriving (Show)
                     
data EType = NormalE
           | DamageE
           | HealthE
           deriving (Show)
          
plusVec :: Vector -> Vector -> Vector
plusVec (x1, y1) (x2, y2) = ((x1 + x2),(y1 + y2))
            
moveSpeed :: Float
moveSpeed = 3
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

getX :: Point -> Float
getX (x,_) = x
getY :: Point -> Float
getY (_,y) = y

normalizeVector :: Vector -> Vector
normalizeVector (f1, f2) = ((f1/l), (f2/l))
                               where l = sqrt(f1*f1 + f2*f2)
                               
movePoint :: Point -> Vector -> Point
movePoint (p1, p2) (v1, v2) = ((p1+v1), (p2+v2))

bulletHit :: Point -> Point -> Float -> Bool
bulletHit (p1, p2) (p3, p4) r =
               (p1 >= (p3 - r)) && (p1 <= (p3 + r)) && (p2 >= (p4 - r)) && (p2 <= (p4 + r))
               
class Moves a where
    position :: a -> Point
    direction:: a -> Vector
    move     :: a -> a
   
instance Moves Player where
    position  (Player {pPos = pos}) = pos
    direction (Player {pDir = dir}) = dir
    move p@(Player {pPos = pos, pDir = dir}) = p {pPos = (movePoint pos dir)}
    
instance Moves Enemy where
    position  (Enemy {ePos = pos}) = pos
    direction (Enemy {eDir = dir}) = dir
--ADJUST
    move (Enemy {ePos = pos, eDir = dir, eHealth = h, eType = t}) = Enemy {ePos = (movePoint pos dir), eDir = dir, eHealth = h, eType = t}
    
instance Moves Bullet where
    position  (Bullet {bPos = pos}) = pos
    direction (Bullet {bDir = dir}) = dir
--ADJUST
    move (Bullet {bPos = pos, bDir = dir}) = Bullet {bPos = (movePoint pos dir), bDir = dir}