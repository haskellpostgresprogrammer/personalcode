module Html (simplepage,
--             Content(Div,Span,Image,Link
--                    ,H1,H2,H3,H4,H5,H6,P,Br
             Content(..),Html(..),Head(..),Body(..),html
            ) where

import Xml  
import Data.List
import qualified Utils as U

simplepage f t l =
  writeFile f $
  html (Html (Head t [""] [""]) (Body ([(H1 t)] ++ l)))

data StandardPage = 
  StandardPage PageTitle JavaScriptFile CssFile
  PageContents

data PageTitle = PageTitle String
data JavaScriptFile = JavaScriptFile String
data CssFile = CssFile String
data PageContents = PageContents [Content]

pagetitle (PageTitle s) = s
javascriptfile (JavaScriptFile s) = [s]
cssfile (CssFile s) = [s]
pagecontents (PageContents l) = l

standardpage (StandardPage t j c b) =
  html 
  (Html 
   (Head (pagetitle t) (javascriptfile j) (cssfile c)) 
   (Body (pagecontents b)))

data Html = Html Head Body
data Head = Head Title [ScriptLink] [CssLink]
data Body = Body [Content]
data Content = 
  Div Id [Class] [Content]
  | Span Id [Class] [Content]
  | Image Url String
  | Link Url String
  | NewTabLink Url String
  | Text String
  | H1 String
  | H2 String
  | H3 String
  | H4 String
  | H5 String
  | H6 String
  | P String
  | Br
type Url = String
type Id = String
type Class = String
type ScriptLink = String
type CssLink = String
type Title = String

-- ex = Html (Head "extitle" [] []) (Body [Br])
-- main = xmlf $ htmlf ex

html (Html h b) = 
  xml (Xml (Tag "html" [] [headf h, body b]))
headf (Head t sl cl) = 
  Tag "head" [] (concat [[Tag "title" [] [PCData t]],
                         map scriptlink sl,
                         map csslink cl])
csslink s = 
  SingleTag "link rel=stylesheet" 
  [Attribute "href" s,Attribute "type" "text/css"] 
scriptlink s = Tag "script" 
  [Attribute "src" s,Attribute "type" "text/javascript"] []
body (Body cl) = Tag "body" [] (map content cl)

content (Div i cls cl) = 
  Tag "div" 
  [Attribute "id" i,
   Attribute "class" (intercalate space cls)] 
  (map content cl)
content (Span i cls cl) = 
  Tag "span" 
  [Attribute "id" i,
   Attribute "class" (intercalate space cls)] 
  (map content cl)
content (Image u s) = 
  SingleTag "img" [Attribute "src" u,Attribute "alt" s] 
content (Link u s) =
  Tag "a" [Attribute "href" u] [PCData s]
content (NewTabLink u s) =
  Tag "a" 
  [Attribute "href" u,Attribute "target" "_blank"] 
  [PCData s]
content (Text s) = PCData s
content (H1 s) = Tag "h1" [] [PCData s]
content (H2 s) = Tag "h2" [] [PCData s]
content (H3 s) = Tag "h3" [] [PCData s]
content (H4 s) = Tag "h4" [] [PCData s]
content (H5 s) = Tag "h5" [] [PCData s]
content (H6 s) = Tag "h6" [] [PCData s]
content (P s) = Tag "p" [] [PCData s]
content (Br) = SingleTag "br" []
