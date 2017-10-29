-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
--import Play

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowACircle x y -> translate x y (color green (circleSolid 10))
                
 
