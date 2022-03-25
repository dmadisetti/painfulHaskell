{- Project Euler
Problem 79
==========


   A common security method used for online banking is to ask the user for
   three random characters from a passcode. For example, if the passcode was
   531278, they may ask for the 2nd, 3rd, and 5th characters; the expected
   reply would be: 317.

   The text file, [1]keylog.txt, contains fifty successful login attempts.

   Given that the three characters are always asked for in order, analyse the
   file so as to determine the shortest possible secret passcode of unknown
   length.


   Visible links
   1. keylog.txt
   Answer: 3ccc6e16d99b21d42948f6d49b90fa30
-}

import           Data.HashMap (Map, empty, keys, elems, insert, insertWith, findWithDefault, toList, lookup)
import           System.IO
import           Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Dag   = Map Char String
type Cache = Map Char String

search :: Dag -> String
search d = foldl greedy "" paths
   where
      paths = elems $ foldl traverse (empty :: Cache) (toList d)
      -- traverse - will blow up if not acyclic
      traverse :: Cache -> (Char, String) -> Cache
      traverse m (k, v) = cacheOrSearch $ Data.HashMap.lookup k m
         where
            cacheOrSearch Nothing = insert k (k:new) m'
            cacheOrSearch _ = m
            new = foldl greedy "" (elems m')
            m' = foldl traverse m (zip v (map byKey v))
      greedy a b
         | length a > length b = a
         | otherwise               = b
      byKey = flip (findWithDefault "") d

process :: [String] -> String
process entries = search dag
   where
      process' :: Dag -> [String] -> Dag
      process' = foldl chain
      chain :: Dag -> String -> Dag
      chain r [a, b, c] = r''
         where
            r' = insertWith (++) a [b] r
            r'' = insertWith (++) b [c] r'
      chain r _ = r
      root = empty :: Dag
      dag = process' root entries

main :: IO ()
main = do
  handle <- openFile "external/euler/files/keylog.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ process $ map trim $ lines contents
  hClose handle