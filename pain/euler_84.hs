{- Project Euler
Problem 84
==========


   In the game, Monopoly, the standard board is set up in the following way:

                GO   A1  CC1  A2  T1  R1  B1  CH1  B2   B3  JAIL
                H2                                          C1
                T2                                          U1
                H1                                          C2
                CH3                                         C3
                R4                                          R2
                G3                                          D1
                CC3                                         CC2
                G2                                          D2
                G1                                          D3
                G2J  F3  U2   F2  F1  R3  E3  E2   CH2  E1  FP

   A player starts on the GO square and adds the scores on two 6-sided dice
   to determine the number of squares they advance in a clockwise direction.
   Without any further rules we would expect to visit each square with equal
   probability: 2.5%. However, landing on G2J (Go To Jail), CC (community
   chest), and CH (chance) changes this distribution.

   In addition to G2J, and one card from each of CC and CH, that orders the
   player to go directly to jail, if a player rolls three consecutive
   doubles, they do not advance the result of their 3rd roll. Instead they
   proceed directly to jail.

   At the beginning of the game, the CC and CH cards are shuffled. When a
   player lands on CC or CH they take a card from the top of the respective
   pile and, after following the instructions, it is returned to the bottom
   of the pile. There are sixteen cards in each pile, but for the purpose of
   this problem we are only concerned with cards that order a movement; any
   instruction not concerned with movement will be ignored and the player
   will remain on the CC/CH square.

     • Community Chest (2/16 cards):

         1. Advance to GO
         2. Go to JAIL

     • Chance (10/16 cards):

         1. Advance to GO
         2. Go to JAIL
         3. Go to C1
         4. Go to E3
         5. Go to H2
         6. Go to R1
         7. Go to next R (railway company)
         8. Go to next R
         9. Go to next U (utility company)
        10. Go back 3 squares.

   The heart of this problem concerns the likelihood of visiting a particular
   square. That is, the probability of finishing at that square after a roll.
   For this reason it should be clear that, with the exception of G2J for
   which the probability of finishing on it is zero, the CH squares will have
   the lowest probabilities, as 5/8 request a movement to another square, and
   it is the final square that the player finishes at on each roll that we
   are interested in. We shall make no distinction between "Just Visiting"
   and being sent to JAIL, and we shall also ignore the rule about requiring
   a double to "get out of jail", assuming that they pay to get out on their
   next turn.

   By starting at GO and numbering the squares sequentially from 00 to 39 we
   can concatenate these two-digit numbers to produce strings that correspond
   with sets of squares.

   Statistically it can be shown that the three most popular squares, in
   order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO
   (3.09%) = Square 00. So these three most popular squares can be listed
   with the six-digit modal string: 102400.

   If, instead of using two 6-sided dice, two 4-sided dice are used, find the
   six-digit modal string.


   Answer: ead3264438ef83a8c2da2e98067b4445
-}

import Numeric.LinearAlgebra
import Data.Maybe
import Data.List

cards = 16
sides = 6 :: Integer
die = 2 :: Integer
cells = 40 :: Integer
-- 3x double rolls brings us to jail.
danger = 2 :: Integer
dims = cells * (danger + 1) :: Integer
sides' = fromInteger sides
die' = fromInteger die

-- Special squares in Monopoly 
shared = 5 -- R1 both a jump too and railstation
special = [11, 24, 39, shared] -- C1 E3 H2 R1
community = [33, 17, 2] -- CC1, CC2, CC3
chance = [7, 22, 36] -- CH1, CH2, CH3
stations  = [15, 25, shared] -- CH1 -> R2, CH2 -> R3, CH3 -> R1
utilities = [12, 28, 12] -- CH1 -> U1, CH2 -> U2, CH1 -> U1 
go2jail = [30] -- G2J
jail = 10 -- JAIL
go = 0 -- GO

-- standard dice roll barring doubles
roll :: Integer -> Double
roll x
   | x > (die * sides)   = 0.0
   | x < die             = 0.0
   | otherwise           = roll'' x - (roll' x)
-- Explicitly doubles
roll' :: Integer -> Double
roll' x
   | x < die              = 0.0
   | x > die * sides      = 0.0
   | x `mod` die > 0      = 0.0
   | otherwise            = 1.0 / (sides' ** die')
-- die roll for 2 die
roll'' :: Integer -> Double
roll'' x
   | x < die          = 0.0
   | x > die * sides  = 0.0
   | otherwise        = ((min x' (2 * sides' + 2 - x')) - 1) / (sides' ** 2)
   where
      x' = fromInteger x

die_roll :: Integer -> Integer -> Double
die_roll i j
   | qj > danger                 = 0
   | qi == danger && j == jail   = (1 / sides') + (ring roll)
   | qj == 0                     = ring roll
   | qi + 1 == qj                = ring roll'
   | otherwise = 0
   where
      -- Needs to wrap around from (cells - 1) -> 0
      ring :: (Integer -> Double) -> Double
      ring d
         | attempt > 0 = attempt
         | otherwise   = (d (mj + cells - mi))
         where attempt = (d (mj - mi))
      mi = (i `mod` cells)
      mj = (j `mod` cells)
      qi = (i `quot` cells)
      qj = (j `quot` cells)

construct_system j' i' = sum [
                           edge go2jail maybe_jail,
                           edge community handle_community,
                           edge chance handle_chance,
                           handle_normal]
   where
      normal = die_roll i j
      i = toInteger $ round i'
      j = toInteger $ round j'
      mj = j `mod` cells
      ring = (+) ((quot i cells + 1) * cells)
      -- Apply edge cases
      edge edges f = sum $ map (\x -> f x (ring x)) $ in_range edges
      -- Check to see whether i can move to special case.
      in_range = filter (\e -> (die_roll i e) + (die_roll i $ ring e) > 0)
      -- If special space, handle residual for landing on that position,
      -- otherwise it's just a normal roll.
      handle_normal
         | elem mj go2jail    = 0
         | elem mj chance     = normal * 6 / cards
         | elem mj community  = normal * 14 / cards
         | otherwise          = normal
      maybe_places places x x'
         | elem j places            = (die_roll i x) 
         | elem j (map ring places) = (die_roll i x')
         | otherwise                = 0
      maybe_place place = maybe_places [place]
      maybe_jail = maybe_place jail
      -- Sum components since nonactive parts should be 0
      handle_community x x' = (maybe_go + jailed) / cards
         where
            maybe_go = (maybe_place go x x')
            jailed = maybe_jail x x'
      handle_chance x x' = communed + (total / cards)
         where
            total = maybe_special + maybe_back + maybe_utilities
                     -- There are 2 rail cards
                     + 2 * maybe_stations
            -- Shares cards with community.
            communed = handle_community x x'
            maybe_special = maybe_places special x x'
            -- we don't have to worry about staying in the ring,
            -- cause the chance cards aren't near go.
            maybe_back = maybe_place (x - 3) x x'
            -- We have the chance spots hardcoded to the next util/station.
            idx = fromJust $ elemIndex x chance
            -- Lookups for the hardcoded position.
            maybe_utilities = maybe_place (utilities !! idx) x x'
            maybe_stations = maybe_place (stations !! idx) x x'

-- Create a (board x danger) by (board x danger) matrix.
-- Spots bounded in [0..cells), are in normal game play,
-- while [cells * i, cells * i + 1) represent the board after i
-- double rolls have been performed. Thus when i == danger, there
-- is also a chance to go to jail. 
game :: Matrix Double
game = build (dims', dims') construct_system
   where dims' = (fromIntegral dims)

probs = [sum $ map (probs' !!) d | d <- indices]
   where
      cells' = fromInteger cells
      dims'  = fromInteger dims
      -- Generate the indices about the modulo ring so we can sum up probs
      -- from the 'doubles' states.
      indices = (map (\x -> filter (\y -> x == flip mod cells' y) board) slots)
      slots = [0..cells' - 1] :: [Int]
      board = [0..dims' - 1]  :: [Int]
      -- Solve the eigen value problem and coeffiecentwise square.
      probs' = map (square . realPart) eigenvector
      eigenvector = (map (toList) (toRows $ tr eigenvectors)) !! idx
      (eigenvalues, eigenvectors) = eig game
      -- This is a bit of a hack, we have to find eigenvectors
      -- that correspond to stability, but we know there's only 1
      -- at 1, so we just look for that.
      idx = fromJust $ elemIndex (foldl1' min scaled_values) scaled_values
      scaled_values = map (square . (-) 1 .  magnitude) (toList eigenvalues)
      square = flip (**) 2

-- Single digit numbers need to be mapped to '0x'
pad :: String -> String
pad [x] = ['0', x]
pad xs = xs

main :: IO ()
main = print answer
   where
      answer :: Int
      answer = read $ intercalate "" top3
      top3 = take 3 ranks
      ranks = map stringifyIndex bySize
      stringifyIndex = pad . show . fromJust . flip elemIndex probs
      bySize = (reverse $ sort probs)