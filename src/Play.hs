module Play where
import Data.List

data World = World {
                 player   :: Player
               , enemies  :: [Enemy]
               , pBullets :: [Bullet]
               , eBullets :: [Bullet]
                   }

data Player = Player {
                 pPos    :: Point
               , pHealth :: Int
                     }
             deriving (Show)
                     
data Enemy = Enemy {
                 ePos    :: Point
               , eHealth :: Int
               , eType   :: EType
                   }
                   
data Point = Pt Float Float
           deriving (Show, Eq)

data Bullet = Bullet {  
                 bPos         :: Point
               , bDirection   :: Vector
               , damageFactor :: Int
                     }
            deriving (Show)
                     
data EType = NormalE
           | DamageE
           | HealthE
           deriving (Show)
           
data Vector = Vec Float Float
            deriving (Show)

normalizeVector :: Vector -> Vector
normalizeVector (Vec f1 f2) = Vec (f1/l) (f2/l)
                               where l = sqrt(f1*f1 + f2*f2)
                               
movePoint :: Point -> Vector -> Point
movePoint (Pt p1 p2) (Vec v1 v2) = Pt (p1+v1) (p2+v2)

shoot :: Point -> Vector -> Int -> [Bullet] -> [Bullet]
shoot p v i bs = Bullet {bPos=p, bDirection=v, damageFactor=i} : bs

bulletHit :: Point -> Point -> Float -> Bool
bulletHit (Pt p1 p2) (Pt p3 p4) r =
               (p1 >= (p3 - r)) && (p1 <= (p3 + r)) && (p2 >= (p4 - r)) && (p2 <= (p4 + r))

data MoveDir = Up
             | Down
             | Left
             | Right
             deriving (Eq)
               
movePlayer :: Player -> MoveDir -> Player
movePlayer (Player {pPos = oldPos}) movd | movd == Up   = Player {pPos = movePoint oldPos (Vec 0  1)}
                                         | movd == Down = Player {pPos = movePoint oldPos (Vec 0 (-1))}
                                         | movd == Play.Left = Player {pPos = movePoint oldPos (Vec (-1) 0)}
                                         | otherwise    = Player {pPos = movePoint oldPos (Vec 1  0)}
