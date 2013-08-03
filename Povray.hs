module Povray 
       (Object(Camera,Light,Text,Plane,
               Union,Intersection,Difference,Merge,
               Cone,Cylinder,Box,Sphere),
        Font(Fixed,Proportional,ProportionalSerif),
        Transform (RotateX,RotateY,RotateZ,
                   TranslateX,TranslateY,TranslateZ,
                   ScaleX,ScaleY,ScaleZ,Scale),
        playSlow,playVerySlow,playNormal,
        createAnimation,createPov,
        standardStudio,standardOutdoors,
        writePic,showPic,writeShowPic) where 

import qualified Utils as U
import PovrayTextures

data Transform = 
  RotateX F | RotateY F | RotateZ F
  | TranslateX F | TranslateY F | TranslateZ F
  | ScaleX F | ScaleY F | ScaleZ F | Scale F
  deriving (Show)

--type Texture = Pt.Texture
type F = Double
data Object =
  Camera F F F F F F
  | Union [Object] [Transform]
  | Intersection [Object] [Transform]
  | Difference [Object] [Transform]
  | Merge [Object] [Transform]
  | Cone Texture [Transform]
  | Cylinder Texture [Transform]
  | Box Texture [Transform]
  | Sphere Texture [Transform]
  | Text String Font Texture [Transform]
  | Light F F F F F F
  | Plane F F F F Texture 
  | SkySphere String
  | AreaLight F F F F F F F F F
  deriving (Show)
data IncludeFile = 
  ColorsFile 
  | SkiesFile 
  | TexturesFile
  | ShapesFile

writePic fname objlist = createPov fname objlist
showPic name = U.displayPic $ name ++ ".png"
writeShowPic fname objlist = do
       writePic fname objlist
       showPic fname

data Font = 
  Fixed | Proportional | ProportionalSerif
  deriving (Show)
font (Fixed) = "crystal.ttf"
font (Proportional) = "cyrvetic.ttf"
font (ProportionalSerif) = "timrom.ttf"


standardOutdoors camera object =
  [theSun,sky,theGround,camera,object]
  where
    theGround = ground LimeGreen
    theSun = AreaLight (-30) 20 (-30) 1 1 1 3 3 0.1
    sky = SkySphere "SkyBlue"

standardStudio height width depth object = 
  [lightBehindLeft,lightBehindRight,lightFrontEnd,
   studioCamera,studioFloor,object]
    where
      behindEnd = -(depth/2)
      frontEnd = depth/2
      leftEnd = -(width/2)
      rightEnd = width/2
      centerHeight = height/2
      centerWidth = 0
      centerDepth = 0
      lightBehindLeft = 
        studioLight leftEnd height behindEnd
      lightBehindRight = 
        studioLight rightEnd height behindEnd
      lightFrontEnd = 
        studioLight centerWidth height frontEnd
      slightly = 0.8
      slightlyBelowHeight = height*slightly
      slightlyFromBehindEnd = behindEnd*slightly
      studioCamera = 
        (Camera centerWidth slightlyBelowHeight
         slightlyFromBehindEnd
         centerWidth centerHeight centerDepth)
      studioFloor = ground White
      studioLight x y z = whiteLight x y z

whiteLight x y z = (Light x y z 1 1 1)
ground color = (Plane 0 1 0 0 color)

transforms tl = U.nljoin $ map transform tl
transform (RotateX f) = tvecx "rotate" f
transform (RotateY f) = tvecy "rotate" f
transform (RotateZ f) = tvecz "rotate" f
transform (TranslateX f) = tvecx "translate" f
transform (TranslateY f) = tvecy "translate" f
transform (TranslateZ f) = tvecz "translate" f
transform (ScaleX f) = U.spacejoin ["scale",vec f 1 1]
transform (ScaleY f) = U.spacejoin ["scale",vec 1 f 1]
transform (ScaleZ f) = U.spacejoin ["scale",vec 1 1 f]
transform (Scale f) = tvec "scale" f
tvecx s n = U.spacejoin [s,vec n 0 0]
tvecy s n = U.spacejoin [s,vec 0 n 0]
tvecz s n = U.spacejoin [s,vec 0 0 n]
tvec s n = U.spacejoin [s,vec n n n]

obj n l = U.spacejoin [n,U.curlies $ U.nljoin l,"\n"]
vec x y z = U.angles $ U.commas $ map show [x,y,z]


include x = "#include \""++x++".inc\""
includefile (ColorsFile) = include "colors"
includefile (SkiesFile) = include "skies"
includefile (TexturesFile) = include "textures"
includefile (ShapesFile) = include "shapes"
includes = 
  U.intercalate "\n" $ map includefile 
  [ColorsFile,SkiesFile,TexturesFile,ShapesFile]


objects ol = concatMap object ol

object (AreaLight f1 f2 f3 c1 c2 c3 f4 f5 av) = 
  obj "light_source" 
  [vec f1 f2 f3,vec c1 c2 c3,
   U.commas 
   ["area_light " ++ (vec 10 0 0),vec 0 0 10,
    show f4,show f5],
   "adaptive " ++ (show av),"jitter"]
object (SkySphere c) = 
  obj "sky_sphere" [obj "pigment" [c]]
object (Camera f1 f2 f3 f4 f5 f6) = 
  obj "camera"
  [U.spacejoin ["location",vec f1 f2 f3]
  ,U.spacejoin ["look_at",vec f4 f5 f6]]
object (Light f1 f2 f3 f4 f5 f6) =
  obj "light_source" [vec f1 f2 f3,vec f4 f5 f6]
object (Plane f1 f2 f3 f4 te) =
  obj "plane" [(vec f1 f2 f3) ++ (show f4),texture te]
object (Union ol tl) = 
  obj "union" [objects ol,transforms tl]
object (Intersection ol tl) = 
  obj "intersection" [objects ol,transforms tl]
object (Difference ol tl) = 
  obj "difference" [objects ol,transforms tl]
object (Merge ol tl) = 
  obj "merge" [objects ol,transforms tl]
object (Box te tl) = 
  obj "box" 
  [U.commas [vec (-0.5) (-0.5) (-0.5),vec 0.5 0.5 0.5]
  ,transforms tl,texture te]
object (Sphere te tl) = 
  obj "sphere" 
  [U.commas [vec 0 0 0,show 1]
  ,transforms tl,texture te]
object (Cylinder te tl) = 
  obj "cylinder" 
  [U.commas [vec 0 (-0.5) 0,vec 0 0.5 0,show 1]
  ,transforms tl,texture te]
object (Cone te tl) = 
  obj "cone" 
  [U.commas [vec 0 (-0.5) 0,show 1]
  ,U.commas [vec 0 0.5 0,show 0]
  ,transforms tl,texture te]
object (Text s f te tl) = 
  obj "text" 
  [U.spacejoin ["ttf",U.quoted $ font f,U.quoted s,
                show 0.1,",",show 0]
  ,transforms tl,texture te]

-- text width including spacing - 5
-- text height - 6.5

--     1.4.1.7 How can I find the size of a                    
--     text object?                                            
                                                            
--     "How can I find the size of a text                      
--     object / center text / justify text?"                   
                                                            
--   * You can use the min_extent() and      *           
--     max_extent() functions to get the                       
--     corners of the bounding box of any                      
--     object. While this is sometimes not                     
--     the actual size of the object, for                      
--     text objects this should be fairly                      
--     accurate, enough to do alignment of               
--     the text object.                                        
--   *                                                         
--     1.4.1.6 How can I   1.4.1.8 How do I                    
--   ●  simulate motion      make extruded   ●     
--           blur?               text?                         



createVideo name =
  U.run "ffmpeg" 
  ["-i",name ++ "%d.png","-y",
   "-vcodec","mpeg4",name++".avi"]

playVideo name speed = do 
  U.run "mplayer" 
    ["-zoom","-fs","-speed",show speed,
     name ++ ".avi"]

playVerySlow name = playVideo name 0.1
playSlow name = playVideo name 0.4
playNormal name = playVideo name 1

delPngPov name num = do
  U.run "rm" [name ++ (show num) ++ ".png"]
  U.run "rm" [name ++ (show num) ++ ".pov"]
  if num == 1
    then do print "deleted png's"
    else delPngPov name (num-1)

createPov name ol = do
  writeFile (name ++ ".pov") 
    (includes ++ "\n\n" ++ (objects ol))
  U.run "povray" 
    ["+I" ++ name ++ ".pov",
     "+O" ++ name ++ ".png",
     "+W640","+H360","-D","+Q9"] 

createAnimation name ol = do
  if (length ol) == 0 then putStrLn "empty list"
    else animationLoop name ol 1
animationLoop name ol count = do
  if (length ol) > 1
    then do
    createPov (name ++ (show count)) (head ol)
    animationLoop name (tail ol) (count + 1)
    else do 
    createPov (name ++ (show count)) (head ol)
    createVideo name     
--    delPngPov name count
    putStrLn "finished"

