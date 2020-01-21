
module Bootstrap1 where

import Data.Graph.Dot
import System.FilePath
import System.IO
import System.Directory
import Control.Monad
import Data.List.Extra
import Data.Char
import Control.Arrow
import System.Environment


---------------------------------------------------------------------
-- DROP COLUMNS

dropColumns dir = mapM_ f =<< listFilesExt ".txt" dir
    where
        f file = do
            src <- readFile' file
            writeFile file $ op src

        op = unlines . map (untabs . take 19 . words) . lines


---------------------------------------------------------------------
-- DYNAMIC COMBINE

dynamicCombine dir = do
    gs <- listGraphs dir
    gs <- mapM (\x -> liftM ((,) $ num x) $ parseDotFile2 x) gs
    let x = snd $ head gs
    x <- return $ x
        {graphAttribs = graphAttribs x -< ("label","Dynamic Combined " ++ dir)
        ,edges = concatMap g $ groupBy f $ sort
                 [DotEdge from to (as -< ("color", colors !! i))
                 |(i,d) <- gs, DotEdge from to as <- edges d]
        }
    writeFile (dir <.> "dot") (show x)
    where
        f (DotEdge a b _) (DotEdge c d _) = a == c && b == d
        g xs | length xs >= fromInteger combineThreshold = [DotEdge from to (as -< ("color","black"))]
            where DotEdge from to as = head xs
        g xs = xs

combineThreshold = 6

num :: FilePath -> Int
num = read . reverse . takeWhile (/= '-') . reverse . dropExtension


---------------------------------------------------------------------
-- DYNAMIC

dynamic file = do
    x <- parseDotFile2 file
    x <- return $ x
        {graphAttribs = graphAttribs x -< ("label",takeBaseName file)
        ,nodes = nub
            [DotNode (snd $ lag ni) (as -< ("label",snd $ lag $ as >- "label"))
            |DotNode ni as <- nodes x]
        ,edges=
            [DotEdge fs ts (as -< ("color",colors !! abs (fl - tl)))
            |DotEdge from to as <- edges x, let (fl,fs) = lag from, let (tl,ts) = lag to]
        }
    writeFile (replaceExtension file "colored.dot") (show x)


lag :: String -> (Int, String)
lag s = (read $ init $ ws !! 1, unwords $ drop 2 ws)
    where ws = words s

---------------------------------------------------------------------
-- BOOTSTRAP      get in directory via console -- ghci banjocode.hs -- bootstrapGroup "c:/emily/" etc

bootstrapGroup dir = do
    files <- listGraphs dir

    filess <- return $ groupBy ((==) `on` f) $ sortBy (compare `on` f) files
    mapM_ (\x -> bootstrapFiles x (show $ f $ head x)) filess
    where
        f = readInt . reverse . takeWhile isDigit . drop 1 . dropWhile (/= '_') . reverse . takeFileName


bootstrapDir dir = do
    files <- listGraphs dir
    bootstrapFiles files (dir <.> "dot")


test = bootstrapFiles ["C:\\Users\\Emily\\Dropbox\\emily\\BanjoStuff\\banjoex\\results\\m56.data1.d3_3\\m56.data1.d3_99\\graph_m56.data1.d3_1.dot"] "C:\\Users\\Emily\\Dropbox\\emily\\BanjoStuff\\banjoex\\m56.data1.d3.txt_combine"

-- HERE: MAIN ENTRY POINT
bootstrapFiles files dest = do
    gs <- mapM parseDotFile2 files
    infs <- mapM parseInfFile [takeDirectory x </> "report" ++ drop 5 (takeBaseName x) <.> "txt" | x <- files]
    let is = map (second mean) $ groupSort $ concat infs
    let es = sort $ linkCounts gs
        tot = length gs
    writeFile (dest <.> "txt") $ unlines [show e ++ "\t" ++ perc tot n ++ "\t" ++ maybe "???" show (lookup e is) | (n,e) <- es]
    let ans = (head gs){edges = map (color tot) es}
    writeFile (dest <.> "dot") $ show ans

parseInfFile :: FilePath -> IO [(DotEdge, Double)]
parseInfFile file = do
    src <- readFile' file
    return [ (DotEdge (f a) (f b) [], readDouble c)
           | ["Influence","score","for",a,"->",b,c] <- map words $ lines src]
    where f = takeWhile (/= ',') . dropWhile (== '(')

mean :: [Double] -> Double
mean x = (sum x)/(toEnum (length x))
    

perc :: Int -> Int -> String
perc tot n = show $ round $ (fromIntegral n * 100 / fromIntegral tot :: Double)


classify :: Int -> Int -> Int
classify tot n = if p < 0.99 then 0 else 1 {- | p < 0.99 = 0
               | p < 0.99 = 1
               | n /= tot = 2
               | n == tot = 3 -}
    where
        p = fromIntegral n / fromIntegral tot


classifyColors = ["cyan","blue","indigo","black"]

color :: Int -> (Int,DotEdge) -> DotEdge
color tot (n,e) = e{edgeAttribs = ("color",v) : edgeAttribs e}
    where v = classifyColors !! classify tot n


infoLinks :: Int -> [(Int,DotEdge)] -> [Int]
infoLinks tot = map (subtract 1 . length) . group . sort . (++) [0,1,2,3] . map (classify tot . fst)


linkCounts :: [DotGraph] -> [(Int,DotEdge)]
linkCounts = map (length &&& head) . group . sort . concatMap edges 


normDotEdge e = e{nodeFrom = min a b, nodeTo = max a b}
    where (a,b) = (nodeFrom e, nodeTo e)

---------------------------------------------------------------------
-- COMMON

parseDotFile2 file = do
    p <- parseDotFile file
    case p of
        Left x -> error $ show x
        Right x -> return x


(-<) :: DotAttribs -> (String,String) -> DotAttribs
xs -< (a,b) = (a,b) : filter (not . eqStr a . fst) xs

eqStr a b = map toLower a == map toLower b


(>-) :: DotAttribs -> String -> String
xs >- a = snd $ head $ filter (eqStr a . fst) xs ++ [("","")]


listGraphs :: FilePath -> IO [FilePath]
listGraphs = listFilesExt ".dot"


listFilesExt :: String -> FilePath -> IO [FilePath]
listFilesExt ext dir = do
    files <- getDirectoryContents dir
    return [dir </> x | x <- files, takeExtension x == ext]


readGraphs :: FilePath -> IO [DotGraph]
readGraphs dir = mapM parseDotFile2 =<< listGraphs dir


readFile' file = do
    h <- openFile file ReadMode
    src <- hGetContents h
    length src `seq` hClose h
    return src


colors = ["black","red","orangered","orange","yellow","greenyellow","lawngreen","green","seagreen","royalblue3","blueviolet","indigo","midnightblue"]


on f g x y = f (g x) (g y)


readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble ('.':xs) = read $ '0':'.':xs
readDouble ('-':'.':xs) = read $ '-':'0':'.':xs
readDouble xs = read xs


untabs = concat . intersperse "\t"
