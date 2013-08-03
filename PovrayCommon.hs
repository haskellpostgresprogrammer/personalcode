module PovrayCommon where

import qualified PovrayTextures as Pt

data CommonObject =
  WhiteLight Location
  | Ground Color
  | StandardOutdoors Camera [Object]
  | StandardStudio Height Width Depth [Object]
    
data Object = 
  Union | Intersection | Difference | Merge
data Height = Height Float    
data Width = Width Float
data Depth = Depth Float
data Location = Location Vector    
data Vector = Vector X Y Z
type X = Float
type Y = Float
type Z = Float
type Color = Pt.Texture
data Camera = Camera Location Direction
data Direction = Direction Vector
