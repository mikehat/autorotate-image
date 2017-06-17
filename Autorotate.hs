{-# LANGUAGE OverloadedStrings , BangPatterns #-}


module Autorotate where


import              Data.Maybe                          ( isNothing , isJust , catMaybes )
import              Data.Ratio                          ( Ratio , (%) , numerator , denominator )
import              Data.List                           ( intersperse , findIndex , transpose , maximumBy )
import              Data.Tuple                          ( swap )
import              Control.Applicative                 ( many , empty )
import              Control.Monad                       ( liftM )
import              Control.Monad.IO.Class              ( liftIO )
import              Control.Monad.Trans.Resource        ( release )
import              System.Environment
import              Text.Printf

import              Data.Text as Text                   ( pack )
import              Data.Text.Encoding                  ( encodeUtf8 )
import qualified    Data.Vector.Storable as Vector
import              Graphics.ImageMagick.MagickWand


-- load an image file, trim, rotate and save it
autorotate
    :: String       -- name of original file
    -> String       -- name of output file
    -> Rational     -- imagemagick fuzz for rotation and trim operations
    -> Rational     -- resize factor to reduce edge detection work
    -> Int          -- slot count for edge detection algorithm
    -> String       -- background color for rotation and trim operations
    -> IO ()
autorotate in_file out_file fuzz resize slots bg_color = withMagickWandGenesis $ do

    let in_file' = pack in_file
        out_file' = pack out_file
        bg_color' = encodeUtf8 $ pack bg_color

    background <- pixelWand
    background `setColor` bg_color'

    (_,original_image) <- magickWand
    readImage original_image in_file'

    depth <- getImageDepth original_image >>= (return . fromIntegral)
    -- to do: is this always the correct maximum intensity?
    -- 2^16 works for my scanned images (8-bit depth)
    let fuzz' = fromRational $ fuzz * (2^(2 * depth))

    trimImage original_image fuzz'

    width <- return . fromIntegral =<< getImageWidth original_image
    height <- return . fromIntegral =<< getImageHeight original_image

    -- duplicating and scaling the image prevents the edge detection algorithms
    -- from taking too much memory and time

    let max_length = 400 -- good for a ghc default 8MB stack
        scale' = minimum [ resize , max_length % height , max_length % width ]
        width' = truncate $ (toRational width) * scale'
        height' = truncate $ (toRational height) * scale'

    (_,scaled_image) <- cloneMagickWand original_image
    resizeImage scaled_image width' height' gaussianFilter 1

    transparentPaintImage scaled_image background 0 fuzz' False

    edges <- getSlopesHaskell scaled_image

    let slopes = concat $ map all_slopes edges
        -- the lowest slope value should be 1 (i.e. a 45 degree rotation)
        -- using 1.2 scale to allow grouping above, but close to, a slope of 1
        -- but 1.0 should work perfectly
        distr = makeDistribution slots 1.2 slopes
        compare_length a b = compare (length a) (length b)
        max_distr = maximumBy compare_length  distr
        avg_m = (sum max_distr) / (fromIntegral $ length max_distr)
        theta = (atan $ fromRational avg_m) * 180 / pi

    -- liftIO $ putStrLn $ "theta: " ++ (show theta)
    rotateImage original_image background theta

    -- using fuzz' * 3 works pretty well; need to trim more aggressively for the final result
    trimImage original_image (fuzz' * 2)

    writeImage original_image (Just out_file')

    return ()


lift_fst (Nothing,s) = Nothing
lift_fst (Just f, s) = Just (f,s)
lift_snd (f,Nothing) = Nothing
lift_snd (f, Just s) = Just (f,s)

-- get the index (x-coord) of the first non-trasparent point in a row
find_left_edges :: [[Bool]] -> [Maybe Integer]
find_left_edges = map (liftM fromIntegral . findIndex id)

-- same idea, but using a row iterator in the image monad
find_left_edgesM it = do
    row <- pixelGetNextIteratorRow it
    case row of Nothing -> return []
                Just row -> do alphas <- Vector.mapM getAlphaQuantum row
                               let edge = maybe Nothing (Just . fromIntegral) $ Vector.findIndex (/= 0) alphas
                               edges <- find_left_edgesM it
                               return $ edge:edges

-- calculate the slope (actually, its reciprocal) of every combination of points in a list of them
all_slopes :: [(Integer,Integer)] -> [Rational]
all_slopes [] = []
all_slopes (pt:pts) = slopes ++ all_slopes pts
    where
        slopes = map (get_slope pt) pts
        get_slope (x1,y1) (x2,y2) = (x1 - x2) % (y1 - y2)

-- gather a list of Rationals into a list of lists, grouping close values
makeDistribution :: Int -> Rational -> [Rational] -> [[Rational]]
makeDistribution resolution scale values = groups
    where
        offset = resolution `div` 2
        res' = fromIntegral resolution
        mk_val_pair v = ((round $ v * res' / scale) + offset , v)
        val_pairs = map mk_val_pair values
        groups = map (\i -> map snd $ filter ((==i) . fst) val_pairs) [0..resolution]


-- turn rows of pixels into a transparency mask
mk_mask it = do
    let mask_row row = do
            alphas <- Vector.mapM getAlphaQuantum row
            let mask = Vector.toList $ Vector.map (/=0) alphas
            masks <- mk_mask it
            return $ mask:masks
    row <- pixelGetNextIteratorRow it
    maybe (return []) mask_row row


-- use a Haskell matrix to do edge detection
getSlopesHaskell scaled_image = do
    mask <- mk_mask =<< (return . snd) =<< pixelIterator scaled_image

    let
        mask' = transpose mask
        mask0 = mask
        mask90 = map reverse mask'
        mask180 = reverse $ map reverse mask
        mask270 = reverse $ mask'

        left_edges   = map lift_fst $ zip (find_left_edges mask0  ) [0..]
        bottom_edges = map lift_fst $ zip (find_left_edges mask90 ) [0..]
        right_edges  = map lift_fst $ zip (find_left_edges mask180) [0..]
        top_edges    = map lift_fst $ zip (find_left_edges mask270) [0..]

    return $ map catMaybes [left_edges, bottom_edges, right_edges, top_edges]


-- use imagemagick flip and flop rather than matrix manipulation
getSlopesFlipFlop background scaled_image = do

    -- writeImage scaled_image (Just "s-left.png")
    left_edges   <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image
    flopImage scaled_image
    flipImage scaled_image
    -- writeImage scaled_image (Just "s-right.png")
    right_edges  <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image

    rotateImage scaled_image background 90
    -- writeImage scaled_image (Just "s-top.png")
    top_edges    <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image

    flopImage scaled_image
    flipImage scaled_image
    -- writeImage scaled_image (Just "s-bottom.png")
    bottom_edges <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image

    return $ map catMaybes [left_edges, bottom_edges, right_edges, top_edges]


-- use imagemagick rotation rather than matrix manipulation
getEdgesRotate background scaled_image = do

    -- writeImage scaled_image (Just "s0.png")
    left_edges   <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image
    rotateImage scaled_image background 90
    -- writeImage scaled_image (Just "s90.png")
    bottom_edges <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image
    rotateImage scaled_image background 90
    -- writeImage scaled_image (Just "s180.png")
    right_edges  <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image
    rotateImage scaled_image background 90
    -- writeImage scaled_image (Just "s270.png")
    top_edges    <- return . map lift_fst =<< return . flip zip [0..] =<< find_left_edgesM =<< return . snd =<< pixelIterator scaled_image

    return $ map catMaybes [left_edges, bottom_edges, right_edges, top_edges]






-- below are some unmaintained ways to help visualize what the algorithm is doing




drawDistribution hw_ratio values = do
    let width = length values
        height = fromIntegral $ fst $ properFraction $ (fromIntegral width) * hw_ratio
        offset = length $ takeWhile ((< 0) . fst) values
        max_value = fromIntegral $ foldl1 max $ map snd values

    liftIO $ putStrLn $ "max_value: " ++ (show max_value)
    (_,w) <- magickWand
    (_,dw) <- drawingWand

    c <- pixelWand
    setColor c "white"

    newImage w width height c

    setStrokeOpacity dw 1
    setStrokeWidth dw 1
    setStrokeAntialias dw True

    setColor c "black"
    let draw_value x v = drawLine dw x' h' x' v'
            where
                x' = fromIntegral x
                h' = fromIntegral height
                v' = fromRational $ ((fromIntegral height) * (fromIntegral v)) % max_value
                --height - ((fromIntegral v) * (fromIntegral height) % max_value)

    mapM (uncurry draw_value) $ zip [0..] $ map snd values

    drawImage w dw
    return w




drawFreqs :: Integer -> Integer -> Rational -> [Rational] -> IO Double
drawFreqs width height scale xs = withMagickWandGenesis $ do
    let xs' = map ((+(width `div` 2)).floor.(/ scale).(*(fromIntegral width))) xs
        freq as a = fromIntegral $ length $ filter (==a) as
        freqs = map (freq xs') [0..(2 * width)]

    let max_snd lhs rhs | (snd rhs) > (snd lhs) = rhs
                        | otherwise             = lhs
        major = foldl1 max_snd $ zip [0..] freqs
        m' = (fst major) + 50
        m = (fromIntegral $ m' - (width `div` 2)) * scale / (fromIntegral width)
        theta = if m == 0 then 0 else atan $ fromRational $ recip m

    let --theta =  2.5 :: Double
        m2 = tan theta
        y_m2 = (floor $ (realToFrac m2) / scale * (fromIntegral width)) + (width `div` 2)

    (_,w) <- magickWand
    (_,dw) <- drawingWand

    c <- pixelWand
    c `setColor` "white"

    newImage w (fromInteger width) (fromInteger height) c

    dw `setStrokeOpacity` 1
    dw `setStrokeWidth` 1
    dw `setStrokeAntialias` True

    c `setColor` "red"
    dw `setStrokeColor` c
    drawLine dw (fromIntegral $ width `div` 2) (fromIntegral height) (fromIntegral $ width `div` 2) 0

    c `setColor` "red"
    dw `setStrokeColor` c
    drawLine dw (fromIntegral $ y_m2) (fromIntegral height) (fromIntegral $ y_m2) 0

    c `setColor` "black"
    dw `setStrokeColor` c

    let v_scale = height % (snd major)

    let draw_freq dw x f = drawLine dw (fromIntegral x) (fromRational ((fromIntegral height) - ((fromIntegral f)*v_scale))) (fromIntegral x) (fromIntegral height)

    mapM (uncurry $ draw_freq dw) $ zip [0..] freqs

    drawImage w dw
    writeImage w (Just "data.png")

    liftIO $ putStrLn $ "m': " ++ (show m')
    liftIO $ putStrLn $ "m: " ++ (show m)
    liftIO $ putStrLn $ "width: " ++ (show $ width `div` 2)
    liftIO $ putStrLn $ "scale: " ++ (show scale)
    liftIO $ putStrLn $ "theta': " ++ (show theta)

    return theta


{- drawEdge coords
 -
 - visualize the left edge
 -
 -}
drawEdge edges = do

    let right_margin = 100
        width = right_margin + (foldl1 max $ map fst $ catMaybes edges)
        height = length edges
        width' = fromIntegral width

    (_,w) <- magickWand
    (_,dw) <- drawingWand
    c <- pixelWand

    setColor c "white"
    newImage w width height c

    setColor c "black"
    setStrokeColor dw c
    setStrokeWidth dw 1
    setStrokeOpacity dw 1

    let draw_edge (x,y) = drawLine dw x' y' width' y'
            where
                x' = fromIntegral x
                y' = fromIntegral y

    mapM draw_edge $ catMaybes edges

    drawImage w dw
    return w

drawMask rows = do
    (_,w) <- magickWand
    (_,dw) <- drawingWand
    black <- pixelWand
    white <- pixelWand

    setColor black "black"
    setColor white "white"

    let height = length rows
        width = length $ head rows

    newImage w width height white

    setStrokeColor dw black
    setStrokeWidth dw 1
    setStrokeOpacity dw 1

    let rows' = zip [0..] rows
        pts = concat $ map (uncurry mk_pts) $ zip [0..] rows
        mk_pts y row = map (uncurry (mk_pt y)) $ zip [0..] row
        mk_pt y x c = ((x,y),c)
        black_pts = filter snd pts
    mapM (\(x,y) -> drawPoint dw (fromIntegral x) (fromIntegral y)) $ map fst $ filter snd pts
    drawImage w dw
    return w
