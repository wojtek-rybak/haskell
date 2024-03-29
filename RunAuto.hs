import System.Environment
import System.Exit
import Text.Read
import Control.Monad
import Data.List
import Data.Maybe

import Auto



newtype Alpha = Alpha Char deriving (Eq)



instance Bounded Alpha where
    minBound = Alpha 'A'
    maxBound = Alpha 'Z'



instance Enum Alpha where
    fromEnum (Alpha c) = fromEnum c
    toEnum n = Alpha (toEnum n)



instance Show Alpha where
    show (Alpha c) = show c



main :: IO ()
main = getArgs >>= parse >>= putStrLn . check



parse :: [FilePath] -> IO String
parse [fname] = readFile fname
parse _ = (putStrLn "Wrong number of params.") >> (exitWith ExitSuccess)



check :: String -> String
check s = if length l >= 4 && hasCorrectStates (init l) then
              case liftM2 accepts parsedAuto parsedWord of
                  Just b -> show b
                  Nothing -> "BAD INPUT"
          else "BAD INPUT"
          where parsedAuto = parseAuto (init l)
                parsedWord = mapM readAlphaM (last l)
                l = filter ("" /=) (lines s)


hasCorrectStates :: [String] -> Bool
hasCorrectStates (n:is:as:tr) = case liftM4 checkStates (readMaybe n)
                                                        (readMaybe is)
                                                        (readMaybe as)
                                                        (Just (extractStates tr))
                                of
                                    Just b -> b
                                    Nothing -> False
hasCorrectStates _ = False



checkStates :: Int -> [Int] -> [Int] -> [Int] -> Bool
checkStates n is as tr = all inRange is &&
                         all inRange as &&
                         all inRange tr
                         where inRange x = x <= n && x > 0



extractStates :: [String] -> [Int]
extractStates = catMaybes . (map readMaybe) . concat . (map words)



parseAuto :: [String] -> Maybe (Auto Alpha Int)
parseAuto (n:init:acc:tr) = liftM4 fromLists (liftM gen (readMaybe n))
                                             (readMaybe init)
                                             (readMaybe acc)
                                             (parseTr tr)
parseAuto _ = Nothing



gen :: Int -> [Int]
gen n = [1..n]



parseTr :: [String] -> Maybe [(Int,Alpha,[Int])]
parseTr = fmap concat . (mapM (parseSingleTr . words))



parseSingleTr :: [String] -> Maybe [(Int,Alpha,[Int])]
parseSingleTr (q:w:qs) = liftM3 buildSingleTr (readMaybe q) 
                                              (mapM readAlphaM w) 
                                              (mapM readMaybe qs)
parseSingleTr _ = Nothing



buildSingleTr :: Int -> [Alpha] -> [Int] -> [(Int,Alpha,[Int])]
buildSingleTr q chars qs = [(q,c,qs) | c <- chars]



readAlphaM :: Char -> Maybe Alpha
readAlphaM c = if c < 'A' || c > 'Z' then Nothing else Just (Alpha c)
