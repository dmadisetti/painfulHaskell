{- Project Euler
Problem 43
==========


   The number, 1406357289, is a 0 to 9 pandigital number because it is made
   up of each of the digits 0 to 9 in some order, but it also has a rather
   interesting sub-string divisibility property.

   Let d[1] be the 1^st digit, d[2] be the 2^nd digit, and so on. In this
   way, we note the following:

     • d[2]d[3]d[4]=406 is divisible by 2
     • d[3]d[4]d[5]=063 is divisible by 3
     • d[4]d[5]d[6]=635 is divisible by 5
     • d[5]d[6]d[7]=357 is divisible by 7
     • d[6]d[7]d[8]=572 is divisible by 11
     • d[7]d[8]d[9]=728 is divisible by 13
     • d[8]d[9]d[10]=289 is divisible by 17

   Find the sum of all 0 to 9 pandigital numbers with this property.


   Answer: 115253b7721af0fdff25cd391dfc70cf
-}

import           Data.HashMap (Map, empty, insertWith, lookup, singleton)
import           Data.List
import           Data.Maybe   (catMaybes)
import           Data.Set     as Set (fromList,
                                      toList)

viable :: [Int]
viable = toList $ fromList $ capture [0..9] 0
  where
    capture ks n
      | n > 100   = [n]
      | otherwise  = n:mconcat (map gen ks)
      where
        next k = n * 10 + k
        gen k = capture (filter (/=k) ks) (next k)


type Trie = Map Char (Map Char String)

toTrie :: Trie -> String -> Trie
toTrie t [c, b, a] = insertWith f a (singleton b c') t
  where
    c' = [c]
    f _ = insertWith union b c'

divisor :: Int -> [String]
divisor n = filter ensure $ map (augment . show) $ filter check viable
  where
    ensure [a, b, c] = not $ a == b || b == c || c == a
    augment n
      | length n < 3  = "0" ++ n
      | otherwise     = n
    check a = a > 10 && mod a n == 0

divisortrie :: Int -> Trie
divisortrie = foldl toTrie empty . divisor

explore :: [Trie] -> String -> Int
explore [] s = read $ x:s
  where
   [x] = "0123456789" \\ s
explore (t:ts) s@(k':k:_) = sum $ map (explore ts) continuations
  where
    continuations = catMaybes $ process $ Data.HashMap.lookup k t
    process :: Maybe (Map Char String) -> [Maybe String]
    process Nothing   = [Nothing]
    process (Just t') = process' $ Data.HashMap.lookup k' t'
    process' :: Maybe String -> [Maybe String]
    process' Nothing   = [Nothing]
    process' (Just s') = map f s'
    f x
      | x `elem` s   = Nothing
      | otherwise  = Just (x:s)


main :: IO ()
main = print $ sum $ map (explore ds) (divisor 17)
  where
    ds = map divisortrie [13, 11, 7, 5, 3, 2]
