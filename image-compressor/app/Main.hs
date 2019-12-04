module Main where

import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.Bool
import Debug.Trace
import Text.Read
import Data.Maybe

type N = String
type E = String
type Path = String
type Point = (Int, Int)
type Color = (Int, Int, Int)
type Clust = (Float, Float, Float)
data Command = Compressor N E Path


data Pixel = Pixel (Point, Color) | Cluster (Clust) deriving (Read, Show)

parseCommand :: [String] -> Maybe Command
parseCommand [n, e, path] = Just (Compressor n e path)
parseCommand [n] = Nothing
parseCommand [n, e] = Nothing
parseCommand _ = Nothing;

printUsage :: IO ()
printUsage = do
    putStrLn "USAGE: ./imageCompressor n e IN"
    putStrLn "\n"
    putStrLn "\tn\tnumber of colors in the final image"
    putStrLn "\te\tconvergence limit"
    putStrLn "\tIN\tpath to the file containing the colors of the pixels"
    exitWith $ ExitFailure 84

parseFile :: [String] -> Maybe [Pixel]
parseFile ls = mbParsedLines
  where mbParsedLines = sequence (map parseLine splitLines)
        splitLines = map words ls
        parseLine [w1, w2] = do
          coords <- readMaybe w1
          color <- readMaybe w2
          return $ Pixel (coords, color)
        parseLine _ = Nothing

runCompressor :: Command -> IO ()
runCompressor (Compressor n e path) = do
    contents <- readFile path
    let linesContent = lines contents
    let pixel = parseFile linesContent
    -- runKMeans pixel
    -- print pixel
    exitWith ExitSuccess

main :: IO ()
main = do
    args <- getArgs
    let mbCmd = parseCommand args
    case mbCmd of
        Just cmd -> runCompressor cmd
        Nothing -> do
            printUsage
    exitWith $ ExitSuccess



-- catchContent :: [Vector] -> [String] -> [Vector]
-- catchContent _ [] = 0
-- catchContent _ [_] = 0
-- catchContent n (x:y:xs) = if n == y then x else catchContent n (y:xs)

-- convertMap :: [String] -> [Vector]
-- convertMap list = do
--     let list_space = map words list
--     let list_integer = map read list :: []
    -- map read list

-- func :: <function type>
-- func <arguments> = 
--     if condition 
--         then <recursive call>
--         else computedValue
-- https://stackoverflow.com/questions/27404063/what-is-the-equivalent-statement-of-a-while-loop-in-haskell

-- parsePixel :: ReadS Pixel -> [Pixel]
-- parsePixel s = reads $ "Pixel "++s

-- forEach :: [String] -> [Pixel] -> [Pixel]
-- forEach [] pixels = pixels
-- forEach (str:rest) pixels = do
--     let newPixels = pixels ++ (parsePixel str)
--     forEach rest newPixels

-- OUT :: = CLUSTER*
-- CLUSTER ::= ’--\n’ COLOR ’\n-\n’ (POINT COLOR ’\n’)*
-- POINT ::= ’(’ int  ’,’ int  ’)’
-- COLOR ::= ’(’ SHORT  ’,’ SHORT  ’,’ SHORT  ’)’
-- SHORT ::=  ’0’..’255’
