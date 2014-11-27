{-# LANGUAGE Haskell2010, LambdaCase #-}

module Main where

import Data.Functor
import Data.List
import Control.Monad
import System.Environment
import Text.Nicify
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Paths_lhs2html

main = getArgs >>= \case
    
    m : args
        | m == "-m" || m == "--markdown"  -> run lhs2md  "md"  args
        | m == "-u" || m == "--unlit"     -> run lhs2hs  "hs"  args
        | m == "-p" || m == "--parsetree" -> run lhs2txt "txt" args

        | m == "-v" || m == "--version"   -> putStrLn "lhs2html v0.99999"
        | m == "-h" || m == "--help"      -> putStrLn "\
            \  -m  --markdown  transform to github flavored markdown (*.md)\n\
            \  -u  --unlint    plain unlit to *.hs\n\
            \  -p  --parsetree show parse tree as *.txt\n\
            \  -h  --help      this help\n\
            \  -v  --version   show version information"

    args -> run lhs2html "htm" args

run processor suffix = \case

    [] -> processor <$> getContents >>= putStrLn
    
    args -> do
        header <- getDataFileName ("data" </> "header" <.> suffix) >>= readFile
        footer <- getDataFileName ("data" </> "footer" <.> suffix) >>= readFile

        let process f = do
                isFile <- doesFileExist f
                isDir  <- doesDirectoryExist f

                when isDir (globDir1 (compile "*.lhs") f >>= mapM_ process)

                when isFile $ do
                    let outfile = f <.> suffix

                    writeFile outfile header
                    processor <$> readFile f >>= appendFile outfile
                    appendFile outfile footer
 
        mapM_ process args

lhs2txt = nicify . show . parse

lhs2html = foldr toHTML "" . parse

lhs2md = concat . intersperse "\n" . filter (not . null) . map toMarkdown . parse
  where
    nl x y = x ++ '\n' : y
    infixr 5 `nl`

    toMarkdown = \case
        Code xs     -> unlines $ ("```haskell" : xs ++ ["```"])
        Para xs     -> unlines xs
        Quote xs    -> unlines xs
        OrdList xs  -> unlines $ map ("1. " ++) xs
        List xs     -> unlines $ map ("* "  ++) xs
        H1 x        -> x `nl` replicate (length x) '=' `nl` ""
        H2 x        -> x `nl` replicate (length x) '-' `nl` ""
        H3 x        -> "### " ++ x `nl` ""
        _ -> []

lhs2hs = unlines . concatMap (\case { Code xs -> xs; _ -> [] }) . parse

parse :: String -> [Object]
parse = process . map identify . lines

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
    '>' : xs -> Code [xs]

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
    
    Para ls -> mkBlock "p" htmlize ls xs
    Code ls -> mkBlock "pre" (spanify . escape) ls xs
    List ls -> mkBlock "ul" (itemize . htmlize) ls xs
    OrdList ls -> mkBlock "ol" (itemize . htmlize) ls xs
    Quote ls -> mkBlock "blockquote" (spanify . escape) ls xs

    H1 ls -> "<h1>" ++ htmlize ls ++ "</h1>\n" ++ xs
    H2 ls -> "<h2>" ++ htmlize ls ++ "</h2>\n" ++ xs
    H3 ls -> "<h3>" ++ htmlize ls ++ "</h3>\n" ++ xs

    _ -> xs

  where

    mkBlock :: String -> (String -> String) -> [String] -> ShowS
    mkBlock tag f ls xs = foldr (++) ("</" ++ tag ++ ">\n" ++ xs) (("<" ++ tag ++ ">") : map' f ls)

    itemize xs = "<li>" ++ xs ++ "</li>"

    spanify [] = ""
    spanify xs = "<span>" ++ xs ++ "</span>"

    map' f = foldr (\a b -> (f a ++ "\n") : b) []

    htmlize = codify False . escape 

    escape = concatMap $ \case
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        x -> [x]

    codify False ('`' : xs) = "<code>"  ++ codify True  xs
    codify True  ('`' : xs) = "</code>" ++ codify False xs
    codify flag  (x   : xs) = x : codify flag xs
    codify _     _          = []

