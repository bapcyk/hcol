module Main where


import           Data.List
import           Data.Maybe            (fromMaybe, isJust)
import           Prelude               hiding (Word)
import           System.Console.GetOpt
import           System.Environment


-- | Stricts to get syntax errors immediately
data CmdOpts = CmdOpts {
  help     :: !Bool,
  ncols    :: !Int, -- numer of columns
  nspaces  :: !Int, -- spaces between columns
  width    :: !Int, -- width of column
  pagesize :: !Int -- lines in page
  } deriving Show


-- | Default values of command line options
defaultCmdOpts :: CmdOpts
defaultCmdOpts = CmdOpts {
  help = False,
  ncols = 2,
  nspaces = 4,
  width = 20,
  pagesize = 24
  }


-- | Definition of program command line options
cmdSyntax :: [OptDescr (CmdOpts -> CmdOpts)]
cmdSyntax =
  [
    Option ['h', '?'] ["help"]
      (NoArg (\cmdOpts -> cmdOpts {help=True} ))
      "print this help",

    Option ['c'] ["cols"]
      (OptArg (\s cmdOpts -> cmdOpts {ncols=readOpt s}) "NCOLS")
      "columns number (default: 2)",

    Option ['s'] ["spaces"]
      (OptArg (\s cmdOpts -> cmdOpts {nspaces=readOpt s}) "NSPACES")
      "spaces number (default: 4)",

    Option ['w'] ["width"]
      (OptArg (\s cmdOpts -> cmdOpts {width=readOpt s}) "WIDTH")
      "column width (default: 20)",

    Option ['p'] ["pagesize"]
      (OptArg (\s cmdOpts -> cmdOpts {pagesize=readOpt s}) "PAGESIZE")
      "page size in lines (default: 25)"
  ] where
  readOpt :: Read a => Maybe [Char] -> a
  readOpt s = (read $ fromMaybe "" s)


-- | Usage string
usage :: String
usage = usageInfo "SYNTAX: col [options...]" cmdSyntax


-- | Parses command line options
parseCmdOpts :: IO CmdOpts
parseCmdOpts = do
  argv <- getArgs
  case getOpt Permute cmdSyntax argv of
    (opts, _, []) -> return $ foldl (flip id) defaultCmdOpts opts
    (_, _, errs) -> error $ concat errs ++ "\n" ++ usage


-- | Splits `xs` on pies with the same size `n`
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as, bs) = splitAt n xs


-- | Typedefs
type Word = String
type Line = String
type Column = [Line]
type Page = [Column]


-- | Cuts line with width `w` from words `ws` by drop some words and adding
-- spaces between kept ones. If it's possible returns (Maybe line, rest-of-words),
-- otherwise (Nothing, all-words)
cutLine :: Int -> [Word] -> (Maybe Line, [Word])
cutLine _ [] = (Just "", [])
cutLine w ws =
  let wl = takeWhile (<=w) $ scanl1 ((+).(+1)) $ map length ws -- additive words lengths ([2,4,5] vs [2,2,1])
      wn = length wl -- words number
      (w0, w1) = splitAt wn ws
      gn = wn - 1 -- gaps number
  in
    case gn of
      -1 -> (Nothing, ws)
      0 -> (Just $ w0!!0, w1)
      _ ->
        let ag = w - last wl -- additional gaps needed
            gw = 1 + ag `div` gn -- gaps width (except last one)
            lgw = gw + (ag `mod` gn) -- last gap width
            gap = replicate gw ' '
            lgap = replicate lgw ' '
            (firstWords, lastWord) = splitAt (wn-1) w0
        in
          (Just $ intercalate gap firstWords ++ lgap ++ lastWord!!0, w1)


-- | Cuts column (list of lines) with width `w` from words `ws`. If it's possible
-- (to reformat words into new width `w`), returns Maybe column, Nothing otherwise
cutColumn :: Int -> [Word] -> Maybe Column
cutColumn w ws =
  let (mbLine, othWords) = cutLine w ws in
    do
      line <- mbLine
      case othWords of
        [] -> return [line]
        _ -> do othLines <- (cutColumn w othWords)
                return $ [line] ++ othLines


-- | Cuts columns with width `w` for page with size `ps` (lines in page) from input
-- text `txt`
cutColumns :: Int -> Int -> String -> Maybe [Column]
cutColumns w ps txt =
  let ws = words txt in
    cutColumn w ws >>= \c -> return $ splitEvery ps c


-- | Cuts pages from early cutted (reformatted) columns `mbCols` when number of columns
-- is `nc`
cutPages :: Int -> Maybe [Column] -> Maybe [Page]
cutPages nc mbCols = mbCols >>= \cols -> return $ splitEvery nc cols


-- | zip for N sequences
zipN :: [[a]] -> [[Maybe a]]
zipN seqs =
  let seqs' = map (\sq -> map Just sq ++ repeat Nothing) seqs in
  takeWhile (any isJust) [[sq!!i | sq <- seqs'] | i <- enumFrom 0]


-- | Folds page `page` with width of column `w` and vertical space `vSpace` into string
foldPage :: String -> Int -> Page -> Column
foldPage vSpace w page =
 let emptyCol = replicate w ' ' in
 map (\c -> intercalate vSpace $ map (fromMaybe emptyCol) c) (zipN page)


-- | Folds pages `pages` with columns of width `w` and page width `pw` into string
foldPages :: String -> Int -> Int -> [Page] -> String
foldPages vSpace w pw pages =
  let showPage i = "- " ++ show i ++ " -"
      pdel = "\n\n"
      colontit i = pdel
                   ++ replicate (pw `div` 2 - length (showPage i) `div` 2) ' '
                   ++ (showPage i) ++ pdel
      enumPages = zip (map (intercalate "\n" . foldPage vSpace w) pages) (enumFrom 1)
  in
    intercalate "" $ map (\(p,i) -> p ++ colontit (i::Integer)) enumPages


-- | Reformats input text `txt` into new text with columns which options are setted
-- in `opts`
col :: CmdOpts -> String -> String
col opts txt =
  let mbCols = cutColumns (width opts) (pagesize opts) txt
      mbPages = cutPages (ncols opts) mbCols
      vSpace = replicate (nspaces opts) ' '
      pageWidth = (ncols opts) * (width opts) + (ncols opts - 1) * (nspaces opts)
  in
  foldPages vSpace (width opts) pageWidth (fromMaybe [] mbPages)


main :: IO()
main = do
  cmdOpts <- parseCmdOpts
  let CmdOpts { help = fHelp } = cmdOpts in
    if fHelp then putStrLn usage
    else do
         inTxt <- getContents
         putStr $ col cmdOpts inTxt
