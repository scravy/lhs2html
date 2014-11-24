{-# LANGUAGE Haskell2010, LambdaCase #-}

module Main where

import Control.Applicative
import System.Environment
import Text.Nicify

main = getArgs >>= \case
    
    [] -> lhs2html <$> getContents >>= putStrLn
    
    args -> flip mapM_ args $ \f -> lhs2html <$> readFile f >>= writeFile (f ++ ".html")
    
preamble = "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \   <meta charset='UTF-8'>\n\
    \<style>\n\
    \  body, html { padding: 0; margin: 0 }\
    \  #nav { position: fixed; top: 0; right: 0; background: rgba(0,0,0,.5); color: white;\
    \         border-bottom-left-radius: 1em; margin: 0; padding: .5em; counter-reset: theLines; }\n\
    \  #nav a { color: yellow; }\n\
    \  #code { width: 850px; margin: 1em auto 1em auto }\n\
    \  p { margin: .5em 0; line-height: 1.4em; text-align: justify }\n\
    \  code { border-radius: .4em; background: #eeeeee; padding: .2em; }\n\
    \  body { font-family: 'Helvetica', 'Arial', sans-serif }\n\
    \  pre { background: #f0f0f0; border: 1px solid black; padding: 0.5em; }\n\
    \  span { counter-increment: theLines }\n\
    \  span:before { content: counter(theLines); position: absolute; \
    \                display: block; text-align: right;\
    \                margin-left: -3em; padding-right: 2em; color: gray; font-size: .83em }\n\
    \  #code:target > p { display: none }\n\
    \  #code:target > pre { display: block; border-top: 0px; border-bottom: 0px; margin: 0; }\n\
    \  #code:target { margin: 0 auto 0 auto } \n\
    \</style>\n\
    \</head>\n\
    \<body>\n\
    \<p id='nav'>\n\
    \<a href='#'>Show all</a> | <a href='#code'>Show only code</a>\n\
    \</p>\n<div id='code'>\n"


lhs2html :: String -> String
lhs2html = (preamble ++) . foldr toHTML "" . parse

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

