module Php where

import Utils
import Html
import Data.List

php s = spaces ["<?",s,"?>"]

phpInfo = php "phpinfo();"
filePut123 = 
  php 
  "file_put_contents('/home/umar/public_html/dataDir/nuts','nuts\n',FILE_APPEND);"

phpRedirect = 
  php "header(\"Location: http://www.google.com\");"
phpServerVar = 
  php "echo var_dump($_SERVER);"
phpGetVar = php "echo var_dump($_GET);"

statement s = s ++ ";"
argList l = parens $ commas l
apply f al = concat [f,argList al]

data Ifs = Ifs [CondStat]
data CondStat = CondStat Cond [Stat]
data Cond = 
  Gt String String 
  | Lt String String
  | Eq String String
data Stat = Stat String

ifs :: Ifs -> String
ifs (Ifs l) = 
  concat $ (pIf $ head l) : (map pElseIf $ tail l)
    where
      pIf (CondStat c sl) = 
        spaces 
        ["if",cond c,
         nljoin $ concat [["{"],map stat sl,["}"]]]
      pElseIf (CondStat c sl) = 
        spaces 
        [" elseif",cond c,
         nljoin $ concat [["{"],map stat sl,["}"]]]

cond :: Cond -> String
cond (Gt a b) = argList [spaces [a,">",b]]
cond (Lt a b) = argList [spaces [a,"<",b]]
cond (Eq a b) = argList [spaces [a,"==",b]]

stat :: Stat -> String
stat (Stat s) = statement s

redirectLocation s = 
  Stat $ apply "header" 
  [quoted $ concat ["Location: http://",s]]

data Redirects = Redirects [Redirect]
data Redirect = Redirect Arg Url
type Arg = String
type Url = String

redirects :: Redirects -> Ifs
redirects (Redirects l) = Ifs (map redirect l)
redirect :: Redirect -> CondStat
redirect (Redirect a u) = 
  CondStat 
  (Eq "$_GET['outboundTrack']" a) 
  [redirectLocation u]

exRedirects = 
  Redirects 
  [Redirect "goog" "google.com",
   Redirect "hackernews" "news.ycombinator.com",
   Redirect "reddit" "reddit.com"]

redirectLinks :: Redirects -> [Content]
redirectLinks (Redirects l) = map redirectLink l
  where
    redirectLink (Redirect a u) = 
      NewTabLink 
      (getEncode localUrl [("outboundTrack",a)]) a
                  
umarHomePage = 
  Html 
  (Head "Umar's Home Page" [] []) 
  (Body 
   [H1 "Umar's Home Page",
    Div "" [] (redirectLinks exRedirects)])

getEncode u al = 
  concat 
  [u,"?",
   intercalate "&" $ map enc al]
    where
      enc x = concat 
              [map encS $ fst x,"=",map encS $ snd x]
      encS c = 
        case c of
          ' ' -> '+'
          x -> x
      
localUrl = "http://localhost/~umar/"
exHome = conkeror localUrl
exGet q v = 
  conkeror $ getEncode localUrl [(q,v)]
  
testfile = "/home/umar/public_html/index.php"

ex = 
  concat 
  [php $ concat [ifs $ redirects exRedirects],
   html $ umarHomePage]

main = do
  writeFile testfile ex
--  exGet "outboundTrack" "hackernews"
  exHome