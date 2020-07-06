{- Project Euler
Problem 51
==========


   By replacing the 1^st digit of the 2-digit number *3, it turns out that
   six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all
   prime.

   By replacing the 3^rd and 4^th digits of 56**3 with the same digit, this
   5-digit number is the first example having seven primes among the ten
   generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
   56773, and 56993. Consequently 56003, being the first member of this
   family, is the smallest prime with this property.

   Find the smallest prime which, by replacing part of the number (not
   necessarily adjacent digits) with the same digit, is part of an eight
   prime value family.


   Answer: e2a8daa5eb919905dadd795593084c22
-}
import           Data.Hashable
import           Data.HashMap  (Map, empty, findWithDefault, insert, insertWith)
import           Helpers       (primes)

type Depth  = Int
type Tree   = Map Branch Next
type Match  = Branch

data Branch = Wildcard | Node Char deriving (Eq, Ord)
data Next   = Leaf Int | Layer Tree
data Search = Result Depth Next

instance Hashable Branch where
  hash Wildcard = hash ' '
  hash (Node c) = hash c
  hashWithSalt i Wildcard = hashWithSalt i ' '
  hashWithSalt i (Node c) = hashWithSalt i c


maxDepth = 8 - 1
findLeaf = findWithDefault (Leaf 1)
upsert = insertWith update
  where
    update :: Next -> Next -> Next
    update (Leaf a) _ = Leaf (a + 1)


explore' cs = explore
  where
    explore wildcard nextNode tree = Result d (Layer t')
      where
        Result d st = investigate cs wildcard layer
        layer = findWithDefault (Layer (empty :: Tree)) nextNode tree
        t' = insert nextNode st tree


investigate :: String -> Match -> Next -> Search
investigate [c] match (Layer t)
  | wildable match   = Result (max d d') (Layer t'')
  | otherwise        = Result d (Layer t')
  where
    wildable Wildcard  = True
    wildable (Node c') = c' == c
    match' = Node c
    leaf@(Leaf d) = findLeaf match' t
    leaf'@(Leaf d') = findLeaf Wildcard t'
    t' = upsert match' leaf t
    t'' = upsert Wildcard leaf' t'


investigate (c:cs) match (Layer t)
  | match == Wildcard  = Result (max wd wd') (Layer wt'')
  | wildable match     = Result (max d d') (Layer t'')
  | otherwise          = Result d (Layer t')
  where
    wildable (Node c') = c' == c
    explore = explore' cs
    match' = Node c
    Result d (Layer t') = explore match match' t
    Result d' (Layer t'') = explore match Wildcard t'
    Result wd (Layer wt') = explore Wildcard match' t
    Result wd' (Layer wt'') = explore match' Wildcard wt'


invertBlock :: [String] -> [String]
invertBlock ns@(p:_) = reverse a ++ b
  where (a, b) = span ((==) (length p) . length) ns


search :: Tree -> [String] -> Int
search t (p:ps@(p':_))
    | depth == maxDepth       = read p
    | length p /= length p'   = search (empty :: Tree) $ invertBlock ps
    | otherwise               = search t' ps
    where
      Result depth (Layer t')       = investigate p Wildcard (Layer t)


main :: IO ()
main = print $ search (empty :: Tree) $ invertBlock $ map show primes
