{-# LANGUAGE Haskell2010, LambdaCase #-}

module Main where

import Data.Functor
import Control.Monad
import System.Environment
import Text.Nicify

import System.FilePath
import Paths_lhs2html

main = getArgs >>= \case
    
    [] -> lhs2html <$> getContents >>= putStrLn
    
    args -> do
        header <- getDataFileName ("data" </> "header.htm") >>= readFile
        footer <- getDataFileName ("data" </> "footer.htm") >>= readFile

        forM_ args $ \f -> do

            let outfile = f ++ ".html"

            writeFile outfile header
            lhs2html <$> readFile f >>= appendFile outfile
            appendFile outfile footer
    
lhs2html :: String -> String
lhs2html = foldr toHTML "" . parse

parse :: String -> [Object]
parse = filter (\x -> x /= Empty && x /= Para []) . process . map identify . lines

data Object =
    Empty
  | Para [String]
  | Quote [String]
  | Code [String]
  | OrdList [String]
  | List [String]
  | H1 String
  | H2 String
  | H3 String
  | H1' | H2' | H3'
 deriving (Eq, Show)


identify :: String -> Object
identify = \case

    '>' : ' ' : xs -> Code [xs]

    '-' : xs
      | all (== '-') xs -> H2'
      
    '-' : ' ' : xs -> List [xs]

    line@('=' : xs)
      | all (== '=') xs -> H1'
      | otherwise -> Para [line]

    '+' : ' ' : xs -> OrdList [xs]

    line@(' ' : xs)
      | all (== ' ') xs -> Empty
      | otherwise -> Quote [line]

    '#' : '#' : '#' : ' ' : xs -> H3 xs
    '#' : '#' : ' ' : xs -> H2 xs
    '#' : ' ' : xs -> H1 xs

    [] -> Empty
    xs -> Para [xs]

process :: [Object] -> [Object]
process = \case
    Empty : xs@(Empty : _) -> process xs
    
    Para x : Para y : xs -> process (Para (x ++ y) : xs)

    Para x : H1' : xs -> process (Para (init x) : H1 (last x) : xs)
    Para x : H2' : xs -> process (Para (init x) : H2 (last x) : xs)
    Para x : H3' : xs -> process (Para (init x) : H3 (last x) : xs)

    OrdList x : OrdList y : xs -> process (OrdList (x ++ y) : xs)
    List x : List y : xs -> process (List (x ++ y) : xs)

    Quote x : Quote y : xs -> process (Quote (x ++ y) : xs)
    Code x : Code y : xs -> process (Code (x ++ y) : xs)

    x : xs -> x : process xs

    [] -> []

toHTML :: Object -> ShowS
toHTML obj xs = case obj of
    
    Para ls -> foldr (++) ("</p>\n" ++ xs)   ("<p>"   : map' htmlize ls)
    Code ls -> foldr (++) ("</pre>\n" ++ xs) ("<pre>" : map' (spanify . escape) ls)

    H1 ls -> "<h1>" ++ htmlize ls ++ "</h1>\n" ++ xs
    H2 ls -> "<h2>" ++ htmlize ls ++ "</h2>\n" ++ xs
    H3 ls -> "<h3>" ++ htmlize ls ++ "</h3>\n" ++ xs

    _ -> xs

spanify xs = "<span>" ++ xs ++ "</span>"

map' :: (String -> String) -> [String] -> [String]
map' f = foldr (\a b -> (f a ++ "\n") : b) []

htmlize :: String -> String
htmlize = codify False . escape 

escape :: String -> String
escape = concatMap $ \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    x -> [x]

codify :: Bool -> String -> String
codify False ('`' : xs) = "<code>"  ++ codify True  xs
codify True  ('`' : xs) = "</code>" ++ codify False xs
codify flag  (x   : xs) = x : codify flag xs
codify _     _          = []

