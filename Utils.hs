module Utils 
       (distinctCount,nub,
        squoted,quoted,curlies,
        angles,spaces,commas,nljoin,pipes,parens,
        semicolons,colons,dots,nlend,underscores,
        shell,wait,runshell,run,
        wget,useragent,urlParams,
        conkeror,
        ls,findFiles,findDirs,gzReadFile,bzReadFile,
        cpFile,rmFile,mvFile,
        epoch,time,localtime,
        readTime,readLocalTime,formatTime,
        s2i,int,float,
        rand,randInts,randFloats,randDoubles) 
       where

import Data.Char
import Data.List
import Data.List.Split
import qualified Codec.Compression.GZip as Gz
import qualified Codec.Compression.BZip as Bz
import qualified Control.Concurrent as Conc
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding.Error as Ee
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as Tio
import qualified Data.Time.Clock as Tc
import qualified Data.Time.Clock.POSIX as Tp
import qualified Data.Time.Format as Tf
import qualified Data.Time.LocalTime as Tl
import qualified Network.HTTP as N
import qualified System.Cmd
import qualified System.IO as IO
import qualified System.Locale
import qualified System.Process as P
import qualified System.Random as R

-- youtube-dl vimeo-downloader.sh movgrab

cpFile f t = shell $ spaces ["cp",f,t]
mvFile f t = shell $ spaces ["mv",f,t]
rmFile f = shell $ spaces ["rm",f]

s2i s = read s :: Int
s2f s = read s :: Float
int s = s2i s
float s = s2f s

displayPic fname = run "xli" ["-fillscreen",fname]
conkeror url = run "conkeror" [url]
wait s = do Conc.threadDelay $ s*1000000

distinctCount l = 
  sort $ map (\x -> (length x,head x)) $ group $ sort l
distinct l = nub $ sort l

readTime f s = 
  Tf.readTime System.Locale.defaultTimeLocale f
  s :: Tc.UTCTime
readLocalTime f s = 
  Tf.readTime System.Locale.defaultTimeLocale f
  s :: Tl.LocalTime
formatTime f s = 
  Tf.formatTime 
  System.Locale.defaultTimeLocale f s 
time = Tc.getCurrentTime
localtime = Tl.getZonedTime
utc2epoch s = 
  s2i $ formatTime "%s" $ utcString2utc s
  where
    utcString2utc x =
      readTime utcformat x
epoch2utc i = 
  formatTime utcformat $ readTime "%s" $ show i
utcformat = "%Y-%m-%d %H:%M:%S%Q UTC"  
epoch = do
  t <- time
  return $ s2i $ formatTime "%s" t
ago i = timeDifference "ago" i
timeLeft i = timeDifference "left" i

timeDifference s i = 
  return $ map (\(x,y) -> spaces [(show x),y,s]) 
  $ secondsIncrements i
secondsIncrements i = 
  [
    (i,"seconds"),
    (i / 60,"minutes"),
    (i / (60*60),"hours"),
    (i / (60*60*24),"days"),
    (i / (60*60*24*7),"weeks"),
    (i / (60*60*24*7*4),"months"),
    (i / (60*60*24*7*4*12),"years")
  ]

ls dir = do
  l <-  shell $ "ls " ++ dir
  return l

urlParams u l = 
  concat [u,"?",intercalate "&" 
                $ map (\(x,y) -> concat [x,"=",map sp y]) l]
    where
      sp s = case s of
        ' ' -> '+'
        x -> x

wget url file log agent = run "wget" 
                    ["--no-check-certificate",url,
                     "--user-agent=" ++ (quoted agent),
                     "-o",log,"-O",file]
useragent = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1.16) Gecko/20120511 Conkeror/0.9.2 (Debian-0.9.2+git100804-1)"

--  # Log in to the server.  This can be done only once.   
--  wget --save-cookies cookies.txt \      
--   --post-data 'user=foo&password=bar' \     
--   http://server.com/auth.php      
          
--  # Now grab the page or pages we care about.    
--  wget --load-cookies cookies.txt \      
--   -p http://server.com/interesting/article.php    
-- up vote         
-- 80 down  You probably need to add       
-- vote --keep-session-cookies parameter as well.    
-- accepted        
--  (This topic would be probably more suited    


runshell c = P.runInteractiveCommand c
exitcode h = P.waitForProcess h
getoutput o = IO.hGetContents o
codelatin1 h = IO.hSetEncoding h IO.latin1

run c args stdin = P.readProcess c args stdin

shell c = do
  (pi,po,pe,h) <- runshell (c)
  codelatin1 po  
  o <- getoutput po
--  e <- getoutput pe
--  print e
  return $ lines o

gzReadFile f = do
  s <- fmap Gz.decompress (B.readFile f)
  return $ map T.unpack $ T.lines 
    $ E.decodeUtf8With Ee.lenientDecode s
bzReadFile f = do
  s <- fmap Bz.decompress (B.readFile f)
  return $ map T.unpack $ T.lines 
    $ E.decodeUtf8With Ee.lenientDecode s

rand x y = do 
  i <- randInts 1 x y
  return $ head i
randInts num min max = do
  gen <- R.newStdGen
  let ns = R.randomRs (min,max) gen :: [Int] 
  return $ take num ns
randFloats num min max = do
  gen <- R.newStdGen
  let ns = R.randomRs (min,max) gen :: [Float]
  return $ take num ns
randDoubles num min max = do
  gen <- R.newStdGen
  let ns = R.randomRs (min,max) gen :: [Double]
  return $ take num ns

dots sl = intercalate "." sl
colons sl = intercalate ":" sl
semicolons sl = intercalate ";" sl
commas sl = intercalate "," sl
spaces sl = intercalate " " sl
nljoin sl = intercalate "\n" sl
angles s = "<" ++ s ++ ">"
parens s = "(" ++ s ++ ")"
squares s = "[" ++ s ++ "]"
curlies s = "{" ++ s ++ "}"
quoted s = "\"" ++ s ++ "\""
squoted s = "'" ++ s ++ "'"
nlend s = s ++ "\n"
pipes sl = intercalate " | " sl
underscores sl = intercalate "_" sl

findDirs dir = shell $ spaces ["find",dir,"-type","d"]
findFiles dir = shell $ spaces ["find",dir,"-type","f"]

data GoogleServices =
  DriveSync 
data Dir =  
  HomeDir | HaskellFiles | LocalPublic | Public
  | StockMarket | Povray 
  deriving Show
data File = File Dir Name deriving Show
data ListCommands a delimiter =
  Sort a | Group a | DistinctCount a | Distinct a
  | NewLinePrint a | Nub a | Transpose a
  | Intercalate delimiter a | Intersperse delimiter a 
  | Split a | SplitEvery Int a
  deriving Show
data TimeCommands =
  ReadTime Format String 
  | FormatTime Format String
  | CurrentTimeUtc | CurrentTimeEpochString
  | Utc2Epoch String | Epoch2Utc String | UtcFormat
  | Ago Seconds | TimeLeft Seconds
  deriving Show
data SystemCommands =
  DisplayPic File | Sleep Seconds -- wait
  | ListDirectory Dir 
  | Wget Url File LogFile Agent
  | RunCommand Name [Argument]
  | RunShell ShellString
  | OpenGzFile File
  deriving Show
data Agent =
  Firefox | Chrome | W3m | UmarAgent 
  | InternetExplorer | RandomAgent
  deriving Show
data RandomCommands =
  RandomInts HowMany MaxInt MinInt
  | RandomFloats HowMany MaxFloat MinFloat
  deriving Show
data StringCommands =
  SubString Sub String 
  | SubStringText Sub String  
  | String2Int String | String2Float String  
  | Angles String | Parens String | Squares String
  | Curlies String 
  | Quoted String | SingleQuoted String
  | NewLineEnd String
  | Commas [String] | Spaces [String] 
  | Newlines [String]
  deriving Show
type ShellString = String
type Format = String
type Name = String
type HowMany = Int
type MaxInt = Int
type MinInt = Int
type MaxFloat = Float
type MinFloat = Float
type Url = String
type LogFile = File
type Seconds = Float    
type Sub = String
type Argument = String
