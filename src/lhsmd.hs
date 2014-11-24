{-# LANGUAGE Haskell2010, LambdaCase #-}

module Main where

import System.Environment
import Control.Applicative
import Text.Nicify

main = getArgs >>= \case
    
    [] -> lhs2html <$> getContents >>= putStrLn
    
    args -> flip mapM_ args $ \f -> lhs2html <$> readFile f >>= writeFile (f ++ ".html")
    

lhs2html :: String -> String
lhs2html = unlines . map (nicify . show . identify) . lines

data Line = Empty | Code String | OrdList String | List String | H1 | H2 | Line String
  deriving (Eq, Show)

identify :: String -> Line
identify line = case dropWhile (== ' ') line of

    '>' : xs -> Code xs
    '-' : xs
      | all (== '-') xs -> H2
      | otherwise -> List xs
    '=' : xs
      | all (== '=') xs -> H1
      | otherwise -> Line line
    '+' : xs -> OrdList xs
    ' ' : xs
      | all (== ' ') -> Empty
      | otherwise -> Quote line

    [] -> Empty


