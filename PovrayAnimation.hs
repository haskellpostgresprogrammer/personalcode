module PovrayAnimation where

import qualified Povray as P

data Animation = 
  Animation CameraPath [LightPath] [ObjectPath]
data Path = Path [Scale] [Rotation] [Translation]
type Scale = Vector
type Rotation = Vector
type Translation = Vector
type Vector = P.Vector
data ObjectPath = ObjectPath Path [Texture]
data LightPath = LightPath Path [Color]
type CameraPath = Path
type Camera = P.Camera
data Light = Light Name
data Object = Object Name
type Name = String
type Texture = String
type Color = Vector
