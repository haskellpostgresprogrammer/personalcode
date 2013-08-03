module Gui where

import HTk.Toplevel.HTk

main :: IO ()
main =
 do main <- initHTk []
    b <- newButton main [text "*"]
    pack b []
    cl <- clicked b
    let count :: Int-> Event ()
        count n =
          do cl >>> (b # text n)
             count (n+1)
    spawnEvent (count 1)
    finishHTk
