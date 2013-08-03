module Svg where

import qualified Data.List as L
import qualified System.Cmd

data Shape = 
  Circle Double Double Double Color
  | Ellipse Double Double Double Double Color
  | Image Double Double Double Double String
  | Line Double Double Double Double Color Double
--   | X Y Double Double Double Double Color
  | Text Double Double String Color Double
  | Rect Double Double Double Double 
    Double Double Color

-- data Shape = 
--   Circle Cx Cy R Fill
--   | Ellipse Cx Cy Rx Ry Fill
--   | Image X Y Width Height File
--   | Line X1 Y1 X2 Y2 Stroke StrokeWidth
--   | X Y Rx Ry Width Height Fill
--   | Text X Y String Fill FontSize
--   | Rect X Y Rx Ry Width Height Fill

-- type Cx = Integer
-- type Cy = Integer
-- type R = Integer
-- type Rx = Integer
-- type Ry = Integer
-- type X = Integer
-- type Y = Integer
-- type Width = Integer
-- type Height = Integer
-- type File = String
-- type X1 = Integer
-- type Y1 = Integer
-- type X2 = Integer
-- type Y2 = Integer
-- type Fill = Color
-- type FontSize = Float
-- type Stroke = Color
-- type StrokeWidth = Float

data Color = Red | Blue | Green | Yellow | Black 
           | White | Gray | Pink | Purple

color Red = "red"
color Blue = "blue"
color Green = "green"
color Yellow = "yellow"
color Black = "black"
color White = "white"
color Gray = "gray"
color Pink = "pink"
color Purple = "purple"

shape (Circle cx cy r fill) = 
  stag ("circle",[("cx",f2s cx),("cy",f2s cy),
                  ("r",f2s r),("fill",color fill)])
shape (Ellipse cx cy rx ry fill) = 
  stag ("ellipse",[("cx",f2s cx),("cy",f2s cy),("rx",f2s rx),
                   ("ry",f2s ry),("fill",color fill)])
shape (Image x y width height file) = 
  stag ("image",[("xlink:href",file),("x",f2s x),("y",f2s y),
                 ("width",f2s width),("height",f2s height)])
shape (Line x1 y1 x2 y2 s sw) =
  stag ("line",[("x1",f2s x1),("y1",f2s y1),("x2",f2s x2),
                ("y2",f2s y2),("stroke",color s),
                ("stroke-width",f2s sw)])  
shape (Rect x y rx ry width height fill) = 
  stag ("line",[("x",f2s x),("y",f2s y),("rx",f2s rx),
                ("ry",f2s ry),("width",f2s width),
                ("height",f2s height),("fill",color fill)])  
shape (Text x y string fill fontsize) = 
  tag ("text",[("x",f2s x),("y",f2s y),("fill",color fill),
               ("font-size",f2s fontsize)],string)

shapes l = concat $ map shape l

brackets s = "<"++s++">"
sbrackets s = brackets $ s ++ "/"
dquote s = "\""++s++"\""
attval (a,v) = a ++ "=" ++ dquote v
attvals l = concat $
            L.intersperse " " $ 
            map attval l
tag (t,l,v) = (brackets $ t ++ " " ++ attvals l)
              ++ v ++ (brackets $ "/" ++ t)
stag (t,l) = sbrackets $ t ++ " " ++ attvals l
i2s i = show i
f2s f = show f

svg s w h = 
  tag ("svg",[("width",i2s w),("height",i2s h)],s)

run c l = do System.Cmd.rawSystem c l

topng f s = do
  writeFile (f ++ ".svg") s
  run "convert"
    ["-background","none",f++".svg",f++".png"]
  run "rm" [f++".svg"]

delpng n = do
  run "rm" ["drawing"++ (show n) ++".png"]
  if n == 0
    then do print "deleted png's"
    else delpng (n-1)
          
createvideo = do
  run "ffmpeg" ["-i","drawing%d.png","-y",
                "-vcodec","mpeg4","drawing.avi"]
  
createimage n s = do  
  topng ("drawing" ++ (show n)) s

playvideo = do 
  run "mplayer" ["drawing.avi"]
  
draw = draw2
draw5 s = svg (shapes s) 1920 1080
draw4 s = svg (shapes s) 1280 720
draw3 s = svg (shapes s) 854 480
draw2 s = svg (shapes s) 640 360
draw1 s = svg (shapes s) 426 240

-- youtube resolutions
--   • 1080p: 1920x1080
--   • 720p: 1280x720
--   • 480p: 854x480
--   • 360p: 640x360
--   • 240p: 426x240

-- ffmpeg -i drawing%d.png -vcodec mpeg4 drawing.avi
  
drawpng f s = topng f $ draw s

-- ex = do
--   drawpng "123" 
--     [(Circle 100 50 40 Red),
--      (Circle 20 20 30 Red),
--      (Text 30 30 "asdf" Blue 30)]

width = 640
height = 360
percent f t = (f/100)*t
pn = percent
pw i = pn width i
ph i = pn height i
