module Xml where

data Xml = Xml Element
data Element 
             = Tag Name [Attribute] [Element]
             | SingleTag Name [Attribute]
             | PCData String
type Name = String
data Attribute = Attribute Key Value
type Key = String
type Value = String

attribute (Attribute k v) = 
  space ++ k ++ "=" ++ (quoted v)
element (Tag n al el) = 
  nl ++ 
  brackets (n ++ (concatMap attribute al)) ++
  concatMap element el ++ nl ++ brackets ("/"++n)
element (SingleTag n al) = 
  nl ++
  brackets (n ++ (concatMap attribute al) ++ "/")
element (PCData s) = nl ++ s
xml (Xml e) = element e

nl = "\n"
brackets x = "<"++x++">"
quoted x = "\""++x++"\""
space = " "
