import System.Environment
import System.Exit
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

tac  = unlines . reverse . lines

parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

