module PovrayTextures 
       (Texture(White,Blue,Red,Green,Black
       ,Wood,Glass,OldGlass,WineBottle
       ,BeerBottle,RubyGlass,DarkGreenGlass
       ,YellowGlass,OrangeGlass,VicksBottleGlass
       ,SoftSilver,NewPenny,TinnyBrass,GoldNugget
       ,ForestGreen,MediumForestGreen,LimeGreen
       ,Aluminum,BrightBronze 
       ,Lightening1,Lightening2
       ,Gray
       ,BrushedAluminum,StarField,ShadowClouds),
       texture) where

import qualified Utils as U

obj (n,l) = n++" {"++ (U.intercalate "\n" l) ++"}"
texturer t = obj ("texture",[t])
pigment c = texturer (obj ("pigment",[c]))

data Texture = 
  White | Blue | Red | Green | Black
  | Wood | Glass | OldGlass | WineBottle
  | BeerBottle | RubyGlass | DarkGreenGlass
  | YellowGlass | OrangeGlass | VicksBottleGlass
  | SoftSilver | NewPenny | TinnyBrass | GoldNugget
  | Aluminum | BrightBronze 
  | Lightening1 | Lightening2
  | BrushedAluminum | StarField | ShadowClouds
  | ForestGreen | MediumForestGreen | LimeGreen
  | Gray
  deriving (Show)

texture White = pigment "White"
texture Blue = pigment "Blue"
texture Red = pigment "Red"
texture Green = pigment "Green"
texture Black = pigment "Black"
texture ForestGreen = pigment "ForestGreen"
texture MediumForestGreen = 
  pigment "MediumForestGreen"
texture LimeGreen = pigment "LimeGreen"
texture Gray = pigment "Gray"

texture Wood = texturer "DMFWood6 "
texture Glass = texturer "NBglass"
texture OldGlass = texturer "NBoldglass"
texture WineBottle = texturer "NBwinebottle"
texture BeerBottle = texturer "NBbeerbottle"
texture RubyGlass = texturer "Ruby_Glass"
texture DarkGreenGlass = 
  texturer "Dark_Green_Glass"
texture YellowGlass = texturer "Yellow_Glass"
texture OrangeGlass = texturer "Orange_Glass"
texture VicksBottleGlass = 
  texturer "Vicks_Bottle_Glass"
texture SoftSilver = texturer "Soft_Silver"
texture NewPenny = texturer "New_Penny"
texture TinnyBrass = texturer "Tinny_Brass"
texture GoldNugget = texturer "Gold_Nugget"
texture Aluminum = texturer "Aluminum"
texture BrightBronze = texturer "Bright_Bronze"
texture Lightening1 = texturer "Lightening1"
texture Lightening2 = texturer "Lightening2"
texture BrushedAluminum = 
  texturer "Brushed_Aluminum"
texture StarField = texturer "Starfield"
texture ShadowClouds = texturer "Shadow_Clouds"
