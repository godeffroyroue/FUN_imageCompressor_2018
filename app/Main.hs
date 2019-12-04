module Main where

import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.Bool
import Debug.Trace
import Text.Read
import Data.Maybe
import Text.Printf

type N = String
type E = String
type Path = String
type Point = (Int, Int)
type Color = (Int, Int, Int)
type Clust = (Float, Float, Float)
data Command = Compressor N E Path

data Pixels = Pixel (Int,Int) (Int,Int,Int) | Centroid (Float,Float,Float) deriving (Read, Show)

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

getPixel :: ReadS Pixels
getPixel s = reads $ "Pixel " ++ s

parsePixel :: [String] -> [Pixels] -> [Pixels]
parsePixel [] list = list
parsePixel (str:rest) list = do
    let pixelStr = getPixel str
    case pixelStr of
        ((Pixel coo color, _):_) -> parsePixel rest (list ++ [(Pixel coo color)])

extractFromList :: Int -> [Pixels] -> [Pixels] -> [Pixels]
extractFromList constanteList [] newList = newList
extractFromList 0 _ newList = newList
extractFromList constanteList (head:rest) newList = extractFromList (constanteList - 1) rest (newList ++ [head])

deleteFromList :: [Pixels] -> [Pixels] -> [Pixels]
deleteFromList [] list = list
deleteFromList (p:rest) (f:list) = deleteFromList rest list


generateCluster :: Int -> Int -> [Pixels] -> [[Pixels]] -> [[Pixels]]
generateCluster 0 _ _ newClusterList = newClusterList
generateCluster constanteList2 constanteList pixelList newClusterList = do
    let newList = extractFromList constanteList pixelList []
    let newpixelList = deleteFromList newList pixelList
    generateCluster (constanteList2 - 1) constanteList newpixelList (newClusterList ++ [newList])


calculCentroid :: [Pixels] -> Int -> Pixels -> Pixels
calculCentroid [] lenghtList (Centroid(r,g,b)) = do
                    Centroid(r/(fromInteger (toInteger lenghtList) :: Float), g/(fromInteger (toInteger lenghtList) :: Float), b/(fromInteger (toInteger lenghtList) :: Float))
calculCentroid (head:rest) lenghtList centroide = do
    case head of
        Pixel coo (r,g,b) -> do
            case centroide of
                Centroid (r2,g2,b2) -> do
                    let r3 = fromInteger (toInteger r) :: Float
                    let g3 = fromInteger (toInteger g) :: Float
                    let b3 = fromInteger (toInteger b) :: Float
                    let newCentroid = (Centroid (r2+r3,g2+g3,b2+b3))
                    calculCentroid rest lenghtList newCentroid
        Centroid (x,y,z) -> do
            calculCentroid rest lenghtList centroide

generateCentroid :: Int -> [[Pixels]] -> [[Pixels]] -> [[Pixels]]
generateCentroid 0 newClusterList clusterList = newClusterList --NewclusterList
generateCentroid numberCentroid newClusterList clusterList = do
    let takeFromList = clusterList !! (numberCentroid - 1) -- number centroid - 1 paske tab commence en [0,1] si centroid = 2
    let addList = tail takeFromList
    let lengthList = (length addList)
    let newCentroid = calculCentroid addList lengthList (Centroid(0, 0, 0))
    let newList = newCentroid : addList
    generateCentroid (numberCentroid - 1) (newClusterList ++ [newList]) clusterList

initCentroid :: Int -> [[Pixels]] -> [[Pixels]] -> [[Pixels]]
initCentroid 0 newClusterList clusterList = newClusterList
initCentroid numberCentroid newClusterList clusterList = do
    let takeFromList = clusterList !! (numberCentroid - 1)
    let lengthList = (length takeFromList) - 1
    let newCentroid = Centroid(0, 0, 0)
    let newList = newCentroid : takeFromList
    initCentroid (numberCentroid - 1) (newClusterList ++ [newList]) clusterList

createEmptyList :: Int -> [[Pixels]] -> [[Pixels]] -> [[Pixels]]
createEmptyList 0 newClusterList clusterList = newClusterList
createEmptyList numberCentroid newClusterList clusterList = do
    let takeFromList = clusterList !! (numberCentroid - 1)
    let centroid = [head takeFromList]
    createEmptyList (numberCentroid - 1) (newClusterList ++ [centroid]) clusterList

sousCalculD :: Pixels -> Int -> Int -> Float -> [[Pixels]] -> Int
sousCalculD _ compteurCluster numerocluster convergence [] = numerocluster
sousCalculD pixeltest compteurCluster numerocluster convergence (tete:rest) = do
    let centroidtest = head tete
    case pixeltest of
        Pixel coord (r, g, b) -> do
            case centroidtest of
                Centroid (r1, g1, b1) -> do
                    let r2 = ((round r1)-r)^2
                    let g2 = ((round g1)-g)^2
                    let b2 = ((round b1)-b)^2
                    let d = sqrt ((fromInteger (toInteger r2) :: Float)+(fromInteger (toInteger g2) :: Float)+(fromInteger (toInteger b2) :: Float))
                    if convergence > d
                        then sousCalculD pixeltest (compteurCluster + 1) compteurCluster d rest
                        else sousCalculD pixeltest (compteurCluster + 1) numerocluster convergence rest


splitList :: Int -> Int -> Int -> [[Pixels]] -> [[Pixels]] -> [[Pixels]]
splitList _ nombreCluster constanteLength [] newList = newList
splitList numeroCluster nombreCluster constanteLength listPixel newList = do
    if nombreCluster == constanteLength
        then splitList numeroCluster nombreCluster constanteLength [] newList
        else if nombreCluster == numeroCluster
            then splitList numeroCluster (nombreCluster + 1) constanteLength listPixel newList
            else splitList numeroCluster (nombreCluster + 1) constanteLength listPixel (newList ++ ([listPixel !! nombreCluster]))


calculD :: Int -> Int -> Int-> [[Pixels]] -> [Pixels] -> [[Pixels]]
calculD _ 0 numeroCluster centroidList _ = centroidList --centroidList
calculD constanteLengthPixel lenghtListPixel numeroCluster centroidList (tete:rest) = do
    let longueurTraduite = (fromInteger (toInteger constanteLengthPixel) :: Float)
    let nbrLisToAdd = sousCalculD tete 0 numeroCluster 1000 centroidList
    let recup = centroidList !! nbrLisToAdd
    let backList = splitList nbrLisToAdd 0 (length centroidList) centroidList []
    let addtoList = recup ++ [tete]
    let newList = backList ++ [addtoList]
    calculD constanteLengthPixel (lenghtListPixel - 1) 0 newList rest

testConvergence :: [[Pixels]] -> [[Pixels]] -> Float -> Int -> Int -> Float
testConvergence [] [] number numberCentroid _ = sqrt number
testConvergence (fa:listAlgo) (fc:centroidList) number numberCentroid counter = do
    case (head fa) of
        Centroid (r1,g1,b1) -> do
            case (head fc) of
                Centroid (r2,g2,b2) -> do
                    let r3 = ((round r2)-(round r1))^2
                    let g3 = ((round g2)-(round g1))^2
                    let b3 = ((round b2)-(round b1))^2
                    let calcul = (r3+g3+b3) + (round number)
                    testConvergence listAlgo centroidList (fromInteger (toInteger calcul) :: Float) numberCentroid (numberCentroid - 1)

runAlgorithm :: Int -> Float -> [Pixels] -> [[Pixels]] -> [[Pixels]] -> [[Pixels]] -> [[Pixels]]
runAlgorithm numberCentroid convergenceNbr pixelList centroidList listCluster [] = do
    let lenghtList = length pixelList
    let listAlgo = calculD lenghtList lenghtList 0 centroidList pixelList
    let test = testConvergence listAlgo centroidList 0 numberCentroid numberCentroid
    let cList = generateCentroid numberCentroid [] listAlgo
    let centroidLList = createEmptyList numberCentroid [] cList
    if test <= convergenceNbr 
        then runAlgorithm numberCentroid convergenceNbr pixelList centroidList listAlgo cList
        else runAlgorithm numberCentroid convergenceNbr pixelList (createEmptyList numberCentroid [] (generateCentroid numberCentroid [] listAlgo)) (generateCentroid numberCentroid [] listAlgo) []
runAlgorithm numberCentroid convergenceNbr pixelList centroidList listCluster newList = newList

printoneList :: [Pixels] -> IO()
printoneList [] = printf "\0"
printoneList (f:rest) = do
    case f of
        Centroid (r1,g1,b1) -> do
            printf "--\n"
            printf "(%.2f,%.2f,%.2f)\n" r1 g1 b1
            printf "-\n"
            printoneList rest
        Pixel (x,y) (r2,g2,b2) -> do
            printf "(%d,%d) (%d,%d,%d)\n" x y r2 g2 b2
            printoneList rest

printList :: [[Pixels]] -> Int -> Int -> IO()
printList [] _ _ = printf "\0"
printList (f:rest) counter nbrCentroid = do
    printoneList f
    printList rest counter nbrCentroid

runCompressor :: Command -> IO ()
runCompressor (Compressor n e path) = do
    contents <- readFile path
    let linesContent = lines contents
    let pixelList = parsePixel linesContent []
    let lengthPixelList = length pixelList
    let numberCentroid = read n :: Int
    let constanteList = div lengthPixelList numberCentroid
    let clusterList = generateCluster numberCentroid (constanteList) pixelList []
    let initCentroidList = initCentroid numberCentroid [] clusterList
    let listCalculated = generateCentroid numberCentroid [] initCentroidList
    let emptyListCentroid = createEmptyList numberCentroid [] listCalculated
    let retourAlgo = runAlgorithm numberCentroid (read e :: Float) pixelList emptyListCentroid listCalculated []
    printList retourAlgo 0 numberCentroid

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