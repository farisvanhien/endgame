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
         
{-         
data Point = Pt Float Float
           deriving (Show, Eq)
-}
           
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
          
{-          
data Vector = Vec Float Float
            deriving (Show)
            -}
          
plusVec :: Vector -> Vector -> Vector
plusVec (x1, y1) (x2, y2) = ((x1 + x2),(y1 + y2))
            
moveSpeed :: Float
moveSpeed = 10
{-
vecRight :: Vector
vecRight = Vec   moveSpeed  0
vecLeft  :: Vector
vecLeft  = Vec (-moveSpeed) 0
vecUp    :: Vector
vecUp    = Vec 0   moveSpeed
vecDown  :: Vector
vecDown  = Vec 0 (-moveSpeed)
-}
vecInit  :: Vector
vecInit  = (0, 0)

normalizeVector :: Vector -> Vector
normalizeVector (f1, f2) = ((f1/l), (f2/l))
                               where l = sqrt(f1*f1 + f2*f2)
                               
movePoint :: Point -> Vector -> Point
movePoint (p1, p2) (v1, v2) = ((p1+v1), (p2+v2))

shoot :: Point -> Vector -> Int -> [Bullet] -> [Bullet]
shoot p v i bs = Bullet {bPos=p, bDir=v, damage=i} : bs

bulletHit :: Point -> Point -> Float -> Bool
bulletHit (p1, p2) (p3, p4) r =
               (p1 >= (p3 - r)) && (p1 <= (p3 + r)) && (p2 >= (p4 - r)) && (p2 <= (p4 + r))

{-
data MoveDir = Up
             | Down
             | Left
             | Right
             deriving (Eq)
               
movePlayerDir :: Player -> MoveDir -> Player
movePlayerDir (Player {pPos = oldPos,pHealth=h}) movd | movd == Up        = Player {pPos = movePoint oldPos (Vec 0  1),   h}
                                                      | movd == Down      = Player {pPos = movePoint oldPos (Vec 0 (-1)), h}
                                                      | movd == Play.Left = Player {pPos = movePoint oldPos (Vec (-1) 0), h}
                                                      | otherwise         = Player {pPos = movePoint oldPos (Vec 1  0),   h}
-}


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
    move (Bullet {bPos = pos, bDir = dir, damage = d}) = Bullet {bPos = (movePoint pos dir), bDir = dir, damage = d}