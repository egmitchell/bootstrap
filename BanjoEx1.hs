
module Main where

import DataFile1
import Bootstrap1

import System.FilePath
import Control.Monad
import Data.List
import Data.Char
import System.Environment
import System.Directory
import Data.Maybe
import System.Process
import System.Exit

data ISinfo = ISinfo
    {edge :: (Int,Int)
    ,counts :: Int
    ,meanIS :: Double
    }
    deriving Show
    
      
main = do
    args <- getArgs
    when (length args < 2) $ do
        error "Not enough arguments, do: banjoex <datafile> <maxruns>"
    let datafile:maxruns:rest = args

    src <- readDataFile datafile

    let maxparents = maybe (calcParents src) read $ takeArg args "maxparents"
    let rep = maybe 1 read $ takeArg args "repeat"
    
    let nsrc = length src

    let bootstrap = case takeArg args "bootstrap" of
            Nothing -> nsrc
            Just y | last y == '%' -> (nsrc * read (init y)) `div` 100
                   | otherwise -> read y

    datafile <- canonicalizePath datafile
    let dataname = takeBaseName datafile
    let prefix = takeDirectory datafile </> "results" </> dataname ++ "_" ++ maxruns </> dataname ++ "_" ++ show bootstrap
    createDirectoryIfMissing True $ prefix </> "samples"

    dots <- forM [1..rep] $ runBanjo prefix datafile src (read maxruns) bootstrap maxparents

    bootstrapFiles dots (prefix </> datafile ++ "_combine") --change so up one level
----------------------------------------------------------------------
mainIS :: FilePath -> IO ([((Int,Int),Int,Double)])    --([[((Int, Int), Double)]])--
mainIS fp = do
    allfiles <- getDirectoryContents fp
    let ss = filter (\x -> takeExtension x == ".txt" ) allfiles
    res<- sequence [readIS (fp </> s)| s <- ss] --fp <\> creates a dir
    let resb = groupIS $ concat res
    let res2 =  summarizeIS resb
    writeISsum (fp ++ "_sumIS.txt") res2
    return(res2)  

groupIS :: [((Int, Int), Double)] -> [[((Int,Int),Double)]] 
groupIS input = groupBy ((==) `on` (fst)) $ map orderEdge $ sort $ input

summarizeIS :: [[((Int, Int), Double)]] -> [((Int,Int),Int,Double)]
summarizeIS [] = []
summarizeIS l = [res] ++ summarizeIS (tail l)
    where
    h1 = head l
    e = fst $ h1!!0
    c =length h1
    m =mean $ map snd h1
    res =(e,c,m) --toISinfo n c m 

writeISsum :: FilePath -> [((Int,Int),Int,Double)] -> IO ()
writeISsum fp x = writeFile fp ("digraph\n{"++concat (map showISsum x)++"}")--undefined -- 

showISsum :: ((Int,Int),Int,Double) -> String
showISsum x = concat $ intersperse "\t" $ [showEdge x] ++["#"] ++ [show (snd3 x)] ++[show (trd3 x)] ++ ["\n"]

showEdge :: ((Int,Int),Int,Double) -> String
showEdge x = concat $ [show (fst (fst3 x))] ++ ["->"] ++ [show (snd(fst3 x))] ++ [";"]

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

toISinfo :: (Int,Int) -> Int -> Double -> ISinfo
toISinfo a b c = ISinfo a b c

orderEdge :: ((Int, Int), Double) -> ((Int, Int), Double)
orderEdge ((a,b),c) = if (a> b) then ((b,a),c) else ((a,b),c) 


readIS :: FilePath -> IO ([((Int,Int),Double)]) --this works
readIS fp = do
    file <- readFile fp
    let res =filter (/=[]) $ map getIS $  map words $ lines  file
        res2 = map isToTriple res
    return (res2)

getIS :: [String] -> [String]
getIS [] = []
getIS x = if head x == ("Influence") then x else []

mainIC :: FilePath -> IO [Double]
mainIC fp = do
    allfiles <- getDirectoryContents fp
    let ss = filter (\x -> takeExtension x == ".txt" ) allfiles
    res<- sequence [readIC (fp </> s)| s <- ss] --fp <\> creates a dir
    let resb = concat $ intersperse "\n" (map show res)
    writeFile (fp ++ "_allIC.txt") resb
    return(res)  

readIC :: FilePath -> IO Double --([((Int,Int),Double)]) 
readIC fp = do
    file <- readFile fp
    let res = concat $ filter (/=[]) $ map getIC $  map words $ lines  file
        res2 = fst $ splitAt 7 $ res!!8
        res3 = read res2 :: Double
    return (res3)
      
getIC :: [String] -> [String]
getIC [] = []
getIC x = if head x == ("label") then x else []

isToTriple :: [String] -> ((Int,Int),Double)
isToTriple  r = ((fst  n1,fst n2),is)
    where
        n1 = read (r!!3) :: (Int,Int)
        n2 =read (r!!5) :: (Int,Int)
        is = read(decTo0 (r!!6)) ::Double 

decTo0 :: String -> String
decTo0 x =  if (head x) == '-' then concat (["-0"] ++ [(tail x)]) else (concat $ ["0."]++[(tail x)])

-----------------------------------------------------------------------------

takeArg :: [String] -> String -> Maybe String
takeArg args x = listToMaybe [drop (length x + 1) a | a <- args, (x ++ "=") `isPrefixOf` a]

    
runBanjo :: String -> String -> DataFile -> Int -> Int -> Int -> Int -> IO FilePath
runBanjo prefix datafile dat maxruns bootstrap maxparents m = do
    putStrLn $ "banjo data=" ++ takeFileName datafile ++ " maxruns=" ++ show maxruns ++
               " bootstrap=" ++ show bootstrap ++ " maxparents=" ++ show maxparents ++
               " #" ++ show m
    let infile = prefix </> "samples" </> takeBaseName datafile ++ "_" ++ show m <.> "txt"
        outfile = prefix </> takeBaseName datafile ++ "_" ++ show m
    dat <- select bootstrap dat
    writeDataFile infile (discrete dat)
    
    let avoid = dropExtension datafile ++ "avoid.str"
    b <- doesFileExist avoid   ---
    when b $ copyFile avoid $ takeDirectory infile </> takeFileName avoid
    let cmd = (++) "cd C:\\Emily\\banjo && java -Xmx200m -jar banjo.jar " $ unwords $
            ["settingsFile=" ++ takeDirectory datafile </> "settings.txt"
            ,"maxProposedNetworks=" ++ show maxruns ++ "000000"
            ,"inputDirectory=" ++ takeDirectory infile
            ,"observationsFile=" ++ takeFileName infile
            ,"outputDirectory=" ++ takeDirectory outfile
            ,"reportFile=report_" ++ takeFileName outfile <.> "txt"
            ,"fileNameForTopGraph=graph_" ++ takeFileName outfile
            ,"maxParentCount=" ++ show maxparents
            ,"maxParentCountForRestart=" ++ show maxparents] ++
            ["mustNotBePresentEdgesFile=" ++ takeFileName avoid | b]
    putStrLn cmd
    res <- system cmd

    when (res /= ExitSuccess) $
        error "Banjo execution failed"
    return $ takeDirectory outfile </> "graph_" ++ takeFileName outfile <.> "dot"
