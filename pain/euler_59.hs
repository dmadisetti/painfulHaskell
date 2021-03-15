{- Project Euler
Problem 59
==========


   Each character on a computer is assigned a unique code and the preferred
   standard is ASCII (American Standard Code for Information Interchange).
   For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

   A modern encryption method is to take a text file, convert the bytes to
   ASCII, then XOR each byte with a given value, taken from a secret key. The
   advantage with the XOR function is that using the same encryption key on
   the cipher text, restores the plain text; for example, 65 XOR 42 = 107,
   then 107 XOR 42 = 65.

   For unbreakable encryption, the key is the same length as the plain text
   message, and the key is made up of random bytes. The user would keep the
   encrypted message and the encryption key in different locations, and
   without both "halves", it is impossible to decrypt the message.

   Unfortunately, this method is impractical for most users, so the modified
   method is to use a password as a key. If the password is shorter than the
   message, which is likely, the key is repeated cyclically throughout the
   message. The balance for this method is using a sufficiently long password
   key for security, but short enough to be memorable.

   Your task has been made easy, as the encryption key consists of three
   lower case characters. Using [1]cipher1.txt, a file containing the
   encrypted ASCII codes, and the knowledge that the plain text must contain
   common English words, decrypt the message and find the sum of the ASCII
   values in the original text.


   Visible links
   1. cipher1.txt
   Answer: 68f891fe214e2bfa07c998ad5d0a390f
-}

import           Data.Char       (ord, chr)
import           Data.List       (sort)
import           Data.Bits       (xor)
import           Data.List.Split
import           System.IO

printable = map ord " etaoinsrhdlucmfywgpbvkxqjz"
freqs :: [Float]
freqs = [0.1831686, 0.1021788, 0.0750999, 0.0655307, 0.0620055, 0.0570308, 0.0573426,
         0.0532627, 0.0497200, 0.0486221, 0.0335617, 0.0335227, 0.0229520, 0.0226509,
         0.0201727, 0.0197181, 0.0168961, 0.0163587, 0.0150312, 0.0146995, 0.0127077,
         0.0078805, 0.0056917, 0.0014981, 0.0011441, 0.0008809, 0.0005979]
passwords = gen ps ps ps
   where
      (_:ps) = printable
      gen :: [Int] -> [Int] -> [Int] -> [[Int]]
      gen [a] [b] [c] = [[a, b, c]]
      gen (a:as) [b] [c] = [a, b, c]:(gen as ps ps)
      gen aa@(a:_) (b:bs) [c] = [a, b, c]:(gen aa bs ps)
      gen aa@(a:_) bb@(b:_) (c:cs) = [a, b, c]:(gen aa bb cs)

expected :: Int -> Int
expected = fromIntegral . round . (*) expect . fromIntegral
   where
      expect = (sum [letter * freq | (letter, freq) <- zip floaty freqs]) / l
      floaty = map fromIntegral printable
      l =  (fromIntegral $ length printable)

computeXOR :: [Int] -> [Int] -> [Int]
computeXOR cipher pass = [xor letter $ pass !! (i `mod` l) | (i, letter) <- zip [0..] cipher]
   where
      l = length pass

getXOR :: [Int] -> [Int]
getXOR cipher = bestAttempt attempts maxBound []
   where
      compute :: [Int] -> Int
      expect = expected $ length cipher
      compute = abs . (-) expect . sum
      attempts_raw = [computeXOR cipher attempt | attempt <- passwords]
      attempts = filter (not . elem 64) $ filter (not . elem 124) $ map (map (min 124)) attempts_raw
      bestAttempt [attempt] best result
         | score < best    = attempt
         | otherwise       = result
         where
            score = compute attempt
      bestAttempt (attempt:rest) best result
         | score < best    = bestAttempt rest score attempt
         | otherwise       = bestAttempt rest best result
         where
            score = compute attempt


process :: [String] -> Int
process = sum . getXOR . map read

main :: IO ()
main = do
  handle <- openFile "external/euler/files/cipher1.txt" ReadMode
  contents <- hGetContents handle
  print $ process (splitOn "," contents)
  hClose handle