-- | This module defines how to turn
--   the game state into a picture
module View where
import Control.Arrow
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Model
--import Play

view :: GameState -> IO Picture
view gs = (combIOPic . sequence)(list)
        where list = showIOString : [(return . viewPure) gs]

viewPure :: GameState -> Picture
viewPure gstate@(GameState {infoToShow = info}) = Pictures (combinePicture info)

combinePicture :: [InfoToShow] -> [Picture]
combinePicture [] = [blank]
combinePicture (x:xs) = (infoToPicture x) : combinePicture xs

combIOPic :: IO [Picture] -> IO Picture
combIOPic a = do p <- a
                 return (Pictures p)

fieldWidth :: Float 
fieldWidth = 400
fieldHeight :: Float
fieldHeight = 400

showIOString :: IO Picture    
showIOString = do file <- readFile "./data/Highscore.txt"
                  return (translate (fieldWidth - 100) (fieldHeight - 30) (scale 0.2 0.2 (color white (text ("HI " ++ file)))))
                 
infoToPicture :: InfoToShow -> Picture
infoToPicture info = case info of
  ShowNothing   -> blank
  ShowANumber x y s n -> translate x y (scale s s (color white (text (show n))))
  ShowAChar   c -> color green (text [c])
  ShowAString x y s -> translate x y (color white (text s))
  ShowACircle x y c r -> translate x y (color c (circleSolid r))            
