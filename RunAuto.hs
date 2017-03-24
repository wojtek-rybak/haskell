import System.Environment
import System.Exit
import Text.Read

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


main = getArgs >>= parse >>= putStr . tac

tac :: String -> String
tac  = unlines . reverse . lines


parse fs = concat `fmap` mapM readFile fs

check :: Maybe (Auto Alpha Int) -> Maybe [Alpha] -> Maybe Bool
check (Just aut) (Just s) = Just (accepts aut s)
check _ _ = Nothing

buildAuto :: [String] -> Maybe (Auto Alpha Int)
buildAuto (n:init:acc:tr) = 
    fromListsM (readMaybe n) (readMaybe init) (readMaybe acc) (parseTr tr)

fromListsM :: Maybe Int -> 
              Maybe [Int] -> 
              Maybe [Int] ->
              Maybe [(Int,Alpha,[Int])] -> Maybe (Auto Alpha Int)

fromListsM _ _ _ _ = Nothing

parseTr :: [String] -> Maybe [(Int,Alpha,[Int])]
parseTr _ = Nothing

parseTrRec :: [[String] -> Maybe [(Int,Alpha,[Int])] -> Maybe [(Int,Alpha,[Int])]


parseSingleTr

 {-
fromParts :: [String] -> Maybe (Auto Alpha Int, String)                                                                                                       
fromParts (n:init:acc:rest) = (fromListsM (readMaybe init) [] [] [], "")
fromParts _ = Nothing
 
fromListsM :: Maybe [Int] -> [Int] -> [Int] -> [(Int,Alpha,[Int])] -> Maybe (Auto Alpha Int) 
fromListsM (Just a) b c d = Just (fromLists a b c d)
fromListsM _ _ _ _ = Nothing


 fromParts (s:ns:un:db:ud:pr:[])                                                                                                                    
 76     = newSessionM (readMaybe s) (readMaybe ns) (Just un) (readMaybe db) (readMaybe ud) (readMaybe pr)                                              
 77 fromParts _ = Nothing   -}
