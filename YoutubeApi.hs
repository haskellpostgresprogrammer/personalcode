module YoutubeApi where

import Utils
import Text.HTML.TagSoup
import Data.List.Split
import Data.List

--   https://gdata.youtube.com/feeds/api/users/computerhistorymuseum/uploads?v=2&max-results=25&start-index=1"
-- youtube api backend for retrieving all videos from
-- a channel i.e. beyond 1000 limit

dir = "/otherdisk2/data/youtubeapiusers/"
yfeed = "https://gdata.youtube.com/feeds/api/users/"
yatts = "/uploads?v=2&max-results=25&start-index="
yurl user index = 
  yfeed ++ (urlencode user) ++ yatts ++ (show index)
    where
      urlencode s = intercalate "+" $ splitOn " " s
logfile user index t = 
  dir ++ 
  (intercalate "--" [user,show index,"log",show t])
datafile user index t = 
  dir ++ 
  (intercalate "--" [user,show index,show t])
yget user index = do
  t <- time
  wget (yurl user index) 
    (datafile user index t) (logfile user index t) ""
  print index

totalAuthor feed = 
  (s2i $ head $ getTexts 
   "<openSearch:totalResults>" feed,
   head $ getTexts "<name>" feed)

ygetAndTotal user = do
  t <- time
  let index = 1
  wget (yurl user index)
    (datafile user index t) (logfile user index t) ""
  f <- readFile (datafile user index t)
  return 
    $ totalAuthor
    $ parseTags f

ygetAll user = do
  ia <- ygetAndTotal user
  let i = fst ia
  mapM (yget user) [26,51..i]
  print "finished"
  
-- downloaded = do 
--   l <-  shell $ "ls "++dir
--   return $ map (split "--") l
-- downloadedUsers = do 
--   l <- downloaded
--   return $ nub $ map (take 1) l
-- downloadedLogs = do  
--   l <- downloaded
--   return $ filter (\x -> "log" == (x !! 2)) l
-- downloadedData = do  
--   l <- downloaded
--   return $ filter (\x -> "log" /= (x !! 2)) l
-- downloadedDataUser user = do
--   t <- epoch
--   l <- downloadedData
--   return
--     $ map (\[x,y] -> (s2i x,t - utc2epoch y)) 
--     $ map tail
--     $ filter (\x -> user == (x !! 0)) l

-- dataToFilename x = ((++) dir) $ intercalate "--" x
-- allFiles = do
--   dl <- downloadedData
--   af <- mapM readFile $ map dataToFilename dl
--   return af
  

-- tags = do
-- --   let t = parseTags $ concat f
-- --   let o = map (\(TagOpen x y) -> (x,(map fst y))) 
-- --           $ filter isTagOpen t
-- --   let c = map (\(TagClose x) -> (x,[""])) 
-- --           $ filter isTagClose t
-- --   mapM print $ sort $ nub $ concat [o,c]  
-- opentags = do
--   f <- allFiles
--   return ""
-- closetags = do
  

getTexts tag l = 
  map (\(TagText x) -> x)
  $ filter isTagText
  $ map (head . drop 1)
  $ sections (~== tag) l

-- getEntries = do
--   f <- allFiles
--   mapM print $ processEntries $ concat f

processEntries l = 
  map processEntry $ partitions (~== "<entry>")
  $ parseTags l
    where
      processEntry e =
        (processEntryTexts e,processEntryAttVals e)

-- totalResults = do
--   f <- allFiles
--   mapM print
--     $ sort $ distinct
--     $ map totalAuthor
--     $ partitions (~== "<feed>")
--     $ parseTags $ concat f

-- feedStarts = do
--   f <- allFiles
--   print
--     $ map indexLengthAuthor
--     $ partitions (~== "<feed>")
--     $ parseTags $ concat f
--       where
--         indexLengthAuthor feed = 
--           (s2i $ head $ getTexts 
--            "<openSearch:startIndex>" feed,
--            length $ partitions (~== "<entry>") feed,
--            head $ getTexts "<name>" feed)

processEntryTexts e =
  map (\(x,y) -> attVal x y) 
  [
   ("Id","yt:videoid"),
   ("Logo","logo"),
   ("Geom Position","gml:pos"),
   ("Category","media:category"),
   ("Description","media:description"),
   ("User Id","yt:userId"),
   ("Uploader Id","yt:uploaderId"),
   ("Title","media:title"),
   ("License","media:license"),
   ("Channel Names","name"),
   ("Channel Urls","uri"),
   ("Published","published"),
   ("Recorded","yt:recorded"),
   ("Updated","yt:uploaded"),
   ("Update","updated"),
   ("Aspect Ratio","yt:aspectRatio")
  ]
    where
      attVal s t = (s,getTexts (br t) e)

processEntryAttVals e =
  map (\(x,y) -> attVals x y)   
  [
    ("Comments","gd:feedLink"),
    ("Rating","gd:rating"),
    ("Link","link"),
    ("Likes DisLikes","yt:rating"),
    ("Counts","yt:statistics"),
    ("Category Label","media:category"),
    ("Category Term","category"),
    ("Media Player","media:player"),
    ("Duration in Seconds","yt:duration"),
    ("Credit","media:credit"),
    ("License","media:license"),
    ("Content Format Urls","media:content"),
    ("Thumbnail","media:thumbnail"),
    ("Access Control","yt:accessControl")
  ]
    where
      attVals s t = 
        map (atts s)
        $ filter (~== (br t))
        $ filter isTagOpen e
      atts s (TagOpen t al) = (s,al)

br s = angles s

data Commands =
  ShowAuthors
  | ShowLastUpdates
  | UpdateAuthor Author
  | DownloadAuthor Author
  | AuthorTotal Author


data FeedFile = 
  FeedFile DirPath Author StartIndex 
  DownloadDateTime deriving Show
data DownloadDateTime = 
  DownloadDateTime String deriving Show
data StartIndex = StartIndex Int deriving Show
data DirPath = DirPath [String] deriving Show
data Author = Author String deriving Show
data Authors = Authors [String] Total deriving Show
type Total = Int

data Feed = Feed [Entry] deriving Show

data Id = Id String deriving Show
data Category = Category String deriving Show
data UserId = Userid String deriving Show
data UploaderId = UploaderId String deriving Show
data Title = Title String deriving Show
data License = License String deriving Show
data ChannelNames = 
  ChannelNames [String] deriving Show
data ChannelUrls = 
  ChannelUrls [String] deriving Show
data Published = 
  Published DateTime deriving Show
data DateTime = DateTime Date Time deriving Show
data Date = Date Year Month Day deriving Show
data Time = 
  Time Hour Minute Second deriving Show
type Hour = Int
type Minute = Int
type Second = Int
type Year = Int
type Month = Int
type Day = Int
data AspectRatio = AspectRatio String deriving Show
                          
data Optional = 
  Logo String
  | GeographicCoordinate Float Float
  | Description String
  | Recorded DateTime
  | Updated DateTime
  | Update DateTime deriving Show

data Entry = 
  Entry Id Category UserId UploaderId Title
  License ChannelNames ChannelUrls
  Published AspectRatio
  [Optional] deriving Show
           
-- processEntryAttVals e =
--     ("Comments","gd:feedLink"),
--     ("Rating","gd:rating"),
--     ("Link","link"),
--     ("Likes DisLikes","yt:rating"),
--     ("Counts","yt:statistics"),
--     ("Category Label","media:category"),
--     ("Category Term","category"),
--     ("Media Player","media:player"),
--     ("Duration in Seconds","yt:duration"),
--     ("Credit","media:credit"),
--     ("License","media:license"),
--     ("Content Format Urls","media:content"),
--     ("Thumbnail","media:thumbnail"),
--     ("Access Control","yt:accessControl")

allFiles = do
  fnames <- findFiles dir
  return $ parseFnames $ removeLogs fnames
    where
      removeLogs = filter (not . isInfixOf "log") 
      parseFnames = map parseFname

authors = do
  feeds <- allFiles
  let unique = nub $ sort $ map author $ feeds
  return $ Authors unique (length unique)
    where
      author (FeedFile _ (Author a) _ _) = a

parseFname fname =
  FeedFile 
  (DirPath $ droplast $ slashes fname)
  (Author $ first fname)
  (StartIndex $ s2i $ second fname)
  (DownloadDateTime $ third fname)
  where
    slashes = splitOn "/"
    dashes = splitOn "--" . last . slashes
    first = head . dashes
    second = head . tail . dashes
    third = last . dashes
    droplast = init
    last = head . reverse
    

