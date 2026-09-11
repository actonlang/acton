{-# LANGUAGE OverloadedStrings #-}
module ScaleReport
  ( ScaleData, ScaleSeries(..), ScaleRecording(..), readScaleRecording, printScaleRecording
  , scaleSeriesReason, addScaleEvent, scaleCharts, scaleSummary, printScaleReport
  , Chart(..), ChartKind(..), ChartPoint(..), chartGuide, chartText, chartPixels, kittyImage, kittyTerminal
  ) where

import Codec.Compression.Zlib (compress)
import Control.Exception (bracket_)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Bits (bit, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (chr, isPrint)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List (foldl', isPrefixOf, intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.FilePath (takeFileName)
import System.IO
import TerminalSize (queryTermSize)
import TestPerf (perfNumber, perfInfo, perfSamplingReason)
import TestFormat (testColorApply, testColorBold)
import Text.Printf (printf)

-- Only accepted curve samples are retained: (point complete, [(wall ms, RSS)]).
-- Diagnostics remain in the journal, not in the chart's in-memory history.
type ScaleData = IM.IntMap (Bool, [(Double, Double)])

data ScaleSeries = ScaleSeries
    { seriesData :: ScaleData
    , seriesInfo :: Maybe Aeson.Object
    , seriesIssue :: Maybe String
    , seriesReason :: Maybe String
    } deriving Show

data ScaleRecording = ScaleRecording
    { recordingPath :: FilePath
    , recordingHeader :: Aeson.Object
    , recordingTests :: M.Map (String, String) ScaleSeries
    , recordingStopped :: Maybe String
    } deriving Show

-- Retain chart values and measurement identity, never the raw diagnostics. Old
-- journals remain viewable even when their identity cannot support comparison.
readScaleRecording :: FilePath -> IO ScaleRecording
readScaleRecording path = withBinaryFile path ReadMode $ \input -> do
    records <- BLC.split '\n' <$> BL.hGetContents input
    case records of
      first:rest -> do
        header <- decode 1 first
        case (KM.lookup "event" header, KM.lookup "version" header) of
          (Just (Aeson.String "study"), Just (Aeson.Number version)) | version `elem` [1, 2, 3] -> do
            let tests = case Aeson.parseMaybe key header of
                  Just name -> M.singleton name emptySeries
                  Nothing -> M.empty
            (reports, stopped) <- readRecords 2 tests Nothing rest
            return (ScaleRecording path header reports stopped)
          _ -> invalid 1 "expected a scaling study journal (version 1, 2 or 3)"
      [] -> invalid 1 "empty scaling journal"
  where
    emptySeries = ScaleSeries IM.empty Nothing Nothing Nothing
    key obj = (,) <$> obj Aeson..: "module" <*> obj Aeson..: "test"
    invalid line message = ioError (userError (path ++ ":" ++ show (line :: Int) ++ ": " ++ message))
    decode line = either (invalid line) return . Aeson.eitherDecode
    readRecords _ reports stopped [] = return (reports, stopped)
    readRecords line reports stopped (record:rest)
      | BL.null record && null rest = return (reports, stopped)
      | otherwise = line `seq` case Aeson.eitherDecode record of
          Left _ | null rest -> do
            hPutStrLn stderr (show path ++ ":" ++ show line ++ ": ignoring an unfinished final record")
            return (reports, stopped)
          Left message -> invalid line message
          Right event -> case KM.lookup "event" event of
            Just (Aeson.String kind) | kind `elem` ["sample_start", "sample", "point", "test_end"] -> do
              name <- either (invalid line) return (Aeson.parseEither key event)
              let previous = M.findWithDefault emptySeries name reports
                  points = addScaleEvent event (seriesData previous)
                  accepted = kind == "sample" && KM.lookup "accepted" event == Just (Aeson.Bool True)
                           && KM.lookup "reference" event /= Just (Aeson.Bool True)
                  info = case KM.lookup "result" event of
                    Just (Aeson.Object result) -> perfInfo result
                    _ -> Nothing
                  identityIssue = case info of
                    Nothing -> Just "performance identity is unavailable"
                    Just current
                      | KM.lookup "scale" current /= KM.lookup "scale" event -> Just "sample workload scale differs from its recorded size"
                      | KM.lookup "scaling" current /= Just (Aeson.Bool True)
                        || KM.lookup "loop" current /= Just (Aeson.Bool True) -> Just "sample is not a scaling measurement"
                      | otherwise -> perfSamplingReason current current
                          <|> (seriesInfo previous >>= (`perfSamplingReason` current))
              reason <- if kind == "test_end"
                then Just <$> either (invalid line) return (Aeson.parseEither (Aeson..: "reason") event)
                else return (seriesReason previous)
              let updated = previous
                    { seriesData = points, seriesReason = reason
                    , seriesInfo = seriesInfo previous <|> if accepted then info else Nothing
                    , seriesIssue = seriesIssue previous <|> if accepted then identityIssue else Nothing
                    }
                  reports' = M.insert name updated reports
              points `seq` seriesInfo updated `seq` seriesIssue updated `seq` reports' `seq` readRecords (line + 1) reports' stopped rest
            Just (Aeson.String "end") -> do
              reason <- either (invalid line) return (Aeson.parseEither (Aeson..: "reason") event)
              readRecords (line + 1) reports (Just reason) rest
            Just (Aeson.String "study") -> invalid line "unexpected second study header"
            _ -> readRecords (line + 1) reports stopped rest

scaleSeriesReason :: ScaleSeries -> ScaleSeries -> Maybe String
scaleSeriesReason old new = seriesIssue old <|> seriesIssue new <|>
    case (seriesInfo old, seriesInfo new) of
      (Just a, Just b) -> perfSamplingReason a b
      _ -> Just "performance identity is unavailable"

printScaleRecording :: Bool -> FilePath -> Maybe FilePath -> IO ()
printScaleRecording useColor path baselinePath = do
    current <- readScaleRecording path
    baseline <- mapM readScaleRecording baselinePath
    let reports = recordingTests current
        pairs = case baseline of
          Nothing -> M.map (\new -> (new, Nothing)) reports
          Just old -> M.intersectionWith (\new previous -> (new, Just previous)) reports (recordingTests old)
    forM_ baseline $ \old -> do
      when (M.null pairs) (ioError (userError "The recordings contain no matching benchmarks"))
      forM_ (M.toAscList pairs) $ \((modName, testName), (new, previous)) ->
        forM_ (previous >>= (`scaleSeriesReason` new)) $ \reason ->
          ioError (userError ("Cannot compare " ++ modName ++ "." ++ testName ++ ": " ++ reason))
      when (M.keys reports /= M.keys (recordingTests old)) $
        putStrLn "Only benchmarks present in both recordings are compared."
    putStrLn ("Recording: " ++ show path)
    forM_ baseline $ \old -> putStrLn ("Baseline: " ++ show (recordingPath old))
    when (all (IM.null . seriesData) (M.elems reports)) $
      putStrLn "No accepted measurements in this recording."
    forM_ (M.toAscList pairs) $ \((modName, testName), (current, old)) -> do
      forM_ baseline $ \previous -> do
        putStrLn ("Current: " ++ clean (takeFileName path))
        putStrLn ("Baseline: " ++ clean (takeFileName (recordingPath previous)))
      printScaleReport useColor (modName ++ "." ++ testName) (seriesData current) (maybe IM.empty seriesData old)
      forM_ (seriesReason current) (putStrLn . ("  " ++) . clean)
      forM_ (old >>= seriesReason) (putStrLn . ("  Baseline: " ++) . clean)
    putStrLn (maybe "Recording is incomplete: no completion event was recorded." (("Stopped: " ++) . clean) (recordingStopped current))
    forM_ baseline $ \old -> putStrLn
      (maybe "Baseline is incomplete: no completion event was recorded." (("Baseline stopped: " ++) . clean) (recordingStopped old))
  where
    clean = map (\c -> if isPrint c then c else ' ')

data ChartPoint = ChartPoint
    { chartScale :: Double
    , chartMinimum :: Double
    , chartMean :: Double
    , chartMaximum :: Double
    , chartComplete :: Bool
    } deriving (Eq, Show)

data ChartKind = WallTime | TimePerScale | PeakMemory deriving (Eq, Show)
data Chart = Chart ChartKind [ChartPoint] [ChartPoint] deriving (Eq, Show)

chartStyle :: ChartKind -> (String, [Int])
chartStyle WallTime = ("Wall time (ms)", [56, 189, 248])
chartStyle TimePerScale = ("Time / scale (µs)", [192, 132, 252])
chartStyle PeakMemory = ("Process peak memory (MiB)", [45, 212, 191])

scaleSummary :: ScaleData -> String
scaleSummary points = case (IM.lookupMin points, IM.lookupMax points) of
    (Just (lo, _), Just (hi, _)) ->
      count complete "size" ++ (if partial == 0 then "" else " + " ++ show partial ++ " partial")
      ++ " · " ++ count samples "curve sample" ++ " · scale " ++ show lo ++ " … " ++ show hi
    _ -> "No measurements"
  where
    complete = length (filter fst (IM.elems points))
    partial = IM.size points - complete
    samples = sum (map (length . snd) (IM.elems points))
    count n noun = show n ++ " " ++ noun ++ if n == 1 then "" else "s"

-- An illustrative proportionality line, not a fit or a complexity verdict.
-- Anchor at the largest completed size; partial samples cannot set the guide.
chartGuide :: Chart -> [(Double, Double)]
chartGuide (Chart WallTime points _) = case (points, reverse (filter chartComplete points)) of
    (first:_, anchor:_) | chartScale first < chartScale anchor ->
      [(chartScale first, chartMean anchor * (chartScale first / chartScale anchor)),
       (chartScale anchor, chartMean anchor)]
    _ -> []
chartGuide _ = []

addScaleEvent :: Aeson.Object -> ScaleData -> ScaleData
addScaleEvent event points = fromMaybe points $ do
    n <- KM.lookup "scale" event >>= Aeson.parseMaybe Aeson.parseJSON
    if n <= 0 then Nothing else case KM.lookup "event" event of
      Just (Aeson.String "point") -> Just (IM.adjust (\(_, xs) -> (True, xs)) n points)
      Just (Aeson.String "sample")
        | KM.lookup "accepted" event == Just (Aeson.Bool True)
        , KM.lookup "reference" event /= Just (Aeson.Bool True)
        , Just (Aeson.Object result) <- KM.lookup "result" event -> do
            wall <- perfNumber result "avg_wall_duration"
            rss <- perfNumber result "peak_rss"
            if wall <= 0 || rss <= 0 then Nothing else
              wall `seq` rss `seq` Just (IM.insertWith
                (\(_, new) (done, old) -> (done, new ++ old)) n (False, [(wall, rss)]) points)
      _ -> Nothing

scaleCharts :: ScaleData -> ScaleData -> [Chart]
scaleCharts points baseline =
    [ chart WallTime (\_ (wall, _) -> wall)
    , chart TimePerScale (\n (wall, _) -> wall * 1000 / n)
    , chart PeakMemory (\_ (_, rss) -> rss / 1048576)
    ]
  where
    chart title value = Chart title (series value points) (series value baseline)
    series value dataPoints =
      [ ChartPoint scale (minimum ys) (sum ys / fromIntegral (length ys)) (maximum ys) done
      | (n, (done, xs)) <- IM.toAscList dataPoints, not (null xs)
      , let scale = fromIntegral n; ys = map (value scale) xs ]

-- Be conservative without querying stdin or consuming the user's keystrokes.
-- Multiplexers get text until their graphics passthrough is supported here.
kittyTerminal :: Bool -> [(String, String)] -> Bool
kittyTerminal tty env = tty && not multiplexer && term /= "dumb"
    && (term `elem` ["xterm-kitty", "xterm-ghostty"] || program `elem` ["kitty", "ghostty"])
  where
    term = fromMaybe "" (lookup "TERM" env)
    program = fromMaybe "" (lookup "TERM_PROGRAM" env)
    multiplexer = any (\key -> maybe False (not . null) (lookup key env)) ["TMUX", "STY"]
               || any (`isPrefixOf` term) ["screen", "tmux"]

printScaleReport :: Bool -> String -> ScaleData -> ScaleData -> IO ()
printScaleReport useColor name points baseline = when (not (IM.null points && IM.null baseline)) $ do
    tty <- hIsTerminalDevice stdout
    (rows, cols) <- if tty then fromMaybe (24, 80) <$> queryTermSize else return (24, 80)
    graphics <- kittyTerminal tty <$> getEnvironment
    -- Leave room for the labels and never move above the visible screen.
    when (rows >= 10 && cols >= 40) $ do
      let width = min 96 (cols - 13)
          height = min 12 (rows - 6)
      let paint = testColorApply useColor
      putStrLn ("\n" ++ paint [testColorBold] ("Scaling charts: " ++ map (\c -> if isPrint c then c else ' ') name))
      putStrLn (scaleSummary points)
      when (not (IM.null baseline)) (putStrLn ("Baseline: " ++ scaleSummary baseline))
      putStrLn "Logarithmic axes; means and min–max ranges; hollow markers = partial points."
      forM_ (scaleCharts points baseline) $ \chart@(Chart kind _ old) -> do
        let rgb = snd (chartStyle kind)
            accent = "\ESC[38;2;" ++ intercalate ";" (map show rgb) ++ "m"
            style row line
              | row == 0 = paint [testColorBold, accent] line
              | row <= height = take 11 line ++ paint [accent] (drop 11 line)
              | otherwise = line
        when (not (null old)) $
          putStrLn (paint [accent] "  ● ━ current" ++ "    " ++ paint [if graphics then "\ESC[38;2;251;146;60m" else accent] "◆ ┄ baseline" ++ "    ◈ overlapping points")
        putStr (unlines (zipWith style [0..] (chartText graphics width height chart)))
        when graphics $ do
          -- The text reserves space first, including when output scrolls.
          -- C=1 keeps the image from moving the cursor; return below the axes.
          -- End any interrupted graphics command before restoring the cursor.
          bracket_ (putStr ("\ESC[" ++ show (height + 2) ++ "A\ESC[12G"))
            (putStr ("\ESC\\\ESC[" ++ show (height + 2) ++ "B\r") >> hFlush stdout)
            (BS.hPut stdout (kittyImage width height (chartPixels useColor width height chart)))
        case chartGuide chart of
          [_, (n, _)] -> putStrLn (paint ["\ESC[38;2;251;191;36m"] ("  ┄ time ∝ scale · anchored at scale " ++ printf "%.0f" n))
          _ -> return ()
        putStrLn ""
      putStrLn "Flat time/scale means roughly linear time over these sizes."
      putStrLn "Peak memory includes process startup, setup, warmup and teardown."
      hFlush stdout

-- Positions are fractions of the plot area. A decade grid keeps small timing
-- differences from looking like large changes. Constant series still have range.
layout :: Chart -> ([(Double, String)], [(Double, String)], [ChartPoint], [ChartPoint], [(Double, Double)])
layout (Chart _ [] []) = ([], [], [], [], [])
layout chart@(Chart _ points baseline) = (ticks xbounds, ticks ybounds, map project points, map project baseline, guide)
  where
    xbounds = bounds (map chartScale (points ++ baseline))
    ybounds = bounds (concatMap (\p -> [chartMinimum p, chartMaximum p]) (points ++ baseline))
    bounds values =
      let lo = logBase 10 (minimum values); hi = logBase 10 (maximum values)
          lower = fromIntegral (floor (lo + 1e-10) :: Int)
          upper = fromIntegral (ceiling (hi - 1e-10) :: Int)
      in if lower >= upper then (lo - 0.5, hi + 0.5) else (lower, upper)
    position (lo, hi) value = (logBase 10 value - lo) / (hi - lo)
    ticks limits@(lo, hi) =
      [ (position limits (10 ** fromIntegral n), if n >= 0 && n <= 3 then show (10^n :: Int) else "1e" ++ show n)
      | n <- [ceiling lo, ceiling lo + max 1 (ceiling ((hi - lo) / 4)) .. floor hi :: Int] ]
    -- Clip before rasterization: clamping every pixel would draw a false line
    -- along the bottom edge wherever the guide lies below the measured range.
    guide = case [(position xbounds n, position ybounds t) | (n, t) <- chartGuide chart] of
      [(ax, ay), (bx, by)] | bx > ax && by > ay ->
        let slope = (by - ay) / (bx - ax)
            left = max ax (ax - ay / slope)
            right = min bx (ax + (1 - ay) / slope)
        in [(u, ay + (u - ax) * slope) | left < right, u <- [left, right]]
      _ -> []
    project p = p { chartScale = position xbounds (chartScale p)
                  , chartMinimum = position ybounds (chartMinimum p)
                  , chartMean = position ybounds (chartMean p)
                  , chartMaximum = position ybounds (chartMaximum p) }

chartText :: Bool -> Int -> Int -> Chart -> [String]
chartText graphics width height chart@(Chart kind raw _) =
    [heading] ++
    [ pad 10 (fromMaybe "" (lookup row labels)) ++ "│" ++ plotRow row | row <- [0..height-1] ] ++
    [replicate 10 ' ' ++ "└" ++ replicate width '─', "     scale " ++ xlabels]
  where
    (xticks, yticks, points, baseline, guide) = layout chart
    title = "  ◆ " ++ fst (chartStyle kind)
    lastValue = case reverse raw of
      p:_ -> "last " ++ number (chartMean p) ++ if chartComplete p then "" else " (partial)"
      _ -> ""
    heading = title ++ if length title + length lastValue + 3 <= width + 11
                        then replicate (width + 11 - length title - length lastValue) ' ' ++ lastValue else ""
    number n | n >= 1e5 || n < 0.001 = printf "%.2e" n
             | otherwise = printf "%.*f" (max 0 (2 - floor (logBase 10 n)) :: Int) n
    labels = [(y height value, label) | (value, label) <- yticks]
    -- Give the end ticks priority and leave a gap between labels on narrow
    -- terminals. Overwriting a neighbouring label could change its number.
    xlabels = foldl' place (replicate width ' ') (case xticks of [] -> []; first:rest -> first : reverse rest)
    place line (value, label) =
      let start = max 0 (min (width - length label) (x width value - length label `div` 2))
          occupied = take (length label + 2) (drop (max 0 (start - 1)) line)
      in if any (/= ' ') occupied then line
         else take start line ++ label ++ drop (start + length label) line
    dots = drawing (2 * width) (4 * height) 2 4 (segments points)
    oldDots = drawing (2 * width) (4 * height) 2 4 (segments baseline)
    guideDots = drawing width height 1 1 (zip guide (drop 1 guide))
    marks = IM.fromList [(y height (chartMean p) * width + x width (chartScale p),
                         if chartComplete p then '●' else '○') | p <- points]
    oldMarks = IM.fromList [(y height (chartMean p) * width + x width (chartScale p), if chartComplete p then '◆' else '◇') | p <- baseline]
    mark i = case (IM.lookup i marks, IM.lookup i oldMarks) of
      (Just '●', Just _) -> Just '◈'
      (Just c, _) -> Just c
      (_, old) -> old
    plotRow row
      | graphics = replicate width ' '
      | otherwise = [fromMaybe (braille col row) (mark (row * width + col)) | col <- [0..width-1]]
    braille col row =
      let bits = foldl' (.|.) 0
            [ bit b | (dx, dy, b) <- [(0,0,0),(0,1,1),(0,2,2),(1,0,3),(1,1,4),(1,2,5),(0,3,6),(1,3,7)]
                    , let i = (row * 4 + dy) * width * 2 + col * 2 + dx
                    , IM.member i dots || ((col + row) `mod` 3 /= 2 && IM.member i oldDots) ]
      in if bits == 0 && col `mod` 3 == 0 && IM.member (row * width + col) guideDots then '·'
         else chr (0x2800 + bits)
    pad n s = replicate (max 0 (n - length s)) ' ' ++ s

x :: Int -> Double -> Int
x size value = max 0 (min (size - 1) (round (value * fromIntegral (size - 1))))

y :: Int -> Double -> Int
y size value = x size (1 - value)

-- One set of line and range geometry feeds both terminal representations.
segments :: [ChartPoint] -> [((Double, Double), (Double, Double))]
segments points = zip centres (drop 1 centres) ++
    [((chartScale p, chartMinimum p), (chartScale p, chartMaximum p)) | p <- points]
  where centres = [(chartScale p, chartMean p) | p <- points]

-- Cell centres align raster marks with the surrounding terminal text.
drawing :: Int -> Int -> Int -> Int -> [((Double, Double), (Double, Double))] -> IM.IntMap ()
drawing width height cellWidth cellHeight edges = IM.fromList
    [(py * width + px, ()) | (a, b) <- edges, (px, py) <- line (pixel a) (pixel b)]
  where
    pixel (u, v) = (round (u * fromIntegral (width - cellWidth) + fromIntegral (cellWidth - 1) / 2),
                    round ((1 - v) * fromIntegral (height - cellHeight) + fromIntegral (cellHeight - 1) / 2))
    line (ax, ay) (bx, by) =
      let steps = max 1 (max (abs (bx - ax)) (abs (by - ay)))
      in [(ax + round (fromIntegral (bx - ax) * fromIntegral i / fromIntegral steps :: Double),
           ay + round (fromIntegral (by - ay) * fromIntegral i / fromIntegral steps :: Double)) | i <- [0..steps]]

chartPixels :: Bool -> Int -> Int -> Chart -> BS.ByteString
chartPixels useColor cols rows chart@(Chart kind _ _) = BS.pack (concatMap pixel [0..width*height-1])
  where
    width = cols * 8
    height = rows * 16
    (xticks, yticks, points, baseline, guide) = layout chart
    -- A seven-pixel horizontal inset keeps the six-pixel highlight inside
    -- the image even when the latest size falls exactly on an axis limit.
    lines = drawing width height 14 16 (segments points)
    oldLines = drawing width height 14 16 (segments baseline)
    guideLine = drawing width height 14 16 (zip guide (drop 1 guide))
    oldMarks = IM.fromList
      [ ((cy + dy) * width + cx + dx, ())
      | p <- baseline
      , let cx = round (chartScale p * fromIntegral (width - 14) + 6.5)
            cy = round ((1 - chartMean p) * fromIntegral (height - 16) + 7.5)
      , dx <- [-5..5], dy <- [-5..5], (if chartComplete p then abs dx + abs dy <= 5 else abs dx + abs dy `elem` [4, 5])
      , cx + dx >= 0, cx + dx < width, cy + dy >= 0, cy + dy < height ]
    marks = IM.fromList
      [ ((cy + dy) * width + cx + dx, alpha)
      | (index, p) <- zip [0..] points
      , let cx = round (chartScale p * fromIntegral (width - 14) + 6.5)
            cy = round ((1 - chartMean p) * fromIntegral (height - 16) + 7.5)
            radius = if index == length points - 1 then 6 else 3
      , dx <- [-radius..radius], dy <- [-radius..radius]
      , cx + dx >= 0, cx + dx < width, cy + dy >= 0, cy + dy < height
      , let distance = dx*dx + dy*dy
      , distance <= 9 || (radius == 6 && distance >= 25 && distance <= 36)
      , let alpha = if distance > 9 then 150 else if distance <= 4 && not (chartComplete p) then 0 else 255 ]
    gridX = [round (v * fromIntegral (width - 14) + 6.5) | (v, _) <- xticks]
    gridY = [round ((1 - v) * fromIntegral (height - 16) + 7.5) | (v, _) <- yticks]
    pixel i = case IM.lookup i marks of
      Just alpha -> accent alpha
      Nothing | IM.member i lines -> accent 255
              | IM.member i oldMarks || (IM.member i oldLines && (i `mod` width - i `div` width) `mod` 12 < 6) -> recorded
              | IM.member i guideLine && (i `mod` width - i `div` width) `mod` 12 < 6 -> gold
              | i `mod` width `elem` gridX || i `div` width `elem` gridY -> [128, 128, 128, 50]
              | otherwise -> [0, 0, 0, 0]
    accent alpha = map fromIntegral (if useColor then snd (chartStyle kind) else [190, 190, 190]) ++ [alpha]
    recorded = (if useColor then [251, 146, 60] else [235, 235, 235]) ++ [255]
    gold = (if useColor then [251, 191, 36] else [150, 150, 150]) ++ [210]

-- Direct transmission works without a shared filesystem (including over SSH).
-- Kitty accepts zlib-compressed RGBA; SVG or a PNG encoder is unnecessary.
kittyImage :: Int -> Int -> BS.ByteString -> BS.ByteString
kittyImage cols rows pixels = chunks metadata encoded
  where
    encoded = Base64.encode (BL.toStrict (compress (BL.fromStrict pixels)))
    metadata = BC.pack ("a=T,f=32,o=z,s=" ++ show (cols * 8) ++ ",v=" ++ show (rows * 16)
                        ++ ",c=" ++ show cols ++ ",r=" ++ show rows ++ ",C=1,")
    chunks prefix bytes =
      let (chunk, rest) = BS.splitAt 4096 bytes
          more = if BS.null rest then "0" else "1"
          command = "\ESC_G" <> prefix <> "q=2,m=" <> more <> ";" <> chunk <> "\ESC\\"
      in if BS.null rest then command else command <> chunks "" rest
