

module Main ( main ) where

import Autorotate

import              Data.Ratio
import              System.Environment
import              System.Exit
import qualified    System.Console.GetOpt as GetOpt
import              Text.Parsec
import              Control.Applicative



main = do
    (opts,params,errs) <- getArgs >>= return . GetOpt.getOpt GetOpt.Permute autorotateOpts

    if wantsHelp opts
        then (putStr $ helpMessage) >> exitSuccess
        else return ()

    if length params /= 2
        then (putStr $ helpMessage) >> exitFailure
        else return ()
    
    let fuzz = getFuzz opts
        resize = getResizeFactor opts
        bg = getBackgroundColor opts
        slots = getSlotCount opts
        (in_file:out_file:_) = params

    autorotate in_file out_file fuzz resize slots bg

    return ()



{- --------------------------------------------------------------------------------
 -
 -                                Command line options
 -}


data AutorotateOption
    = OptionFuzz String
    | OptionResizeFactor String
    | OptionBackgroundColor String
    | OptionSlotCount String
    | OptionHelp
    deriving ( Eq , Show )

defaultFuzz = 1 % 10
defaultResizeFactor = 1 % 3
defaultSlotCount = 120

wantsHelp = any (== OptionHelp)

getFuzz [] = defaultFuzz
getFuzz ((OptionFuzz s):_) = either (const $ error $ "invalid fuzz option: " ++ s) id $ parse fuzz_param "" s
getFuzz (_:opts) = getFuzz opts

getResizeFactor [] = defaultResizeFactor
getResizeFactor ((OptionResizeFactor s):_) = either (const $ error $ "invalid resize value: " ++ s) id $ parse resize_param "" s
getResizeFactor (_:opts) = getResizeFactor opts

getBackgroundColor [] = "gray"
getBackgroundColor ((OptionBackgroundColor s):_) = s
getBackgroundColor (_:opts) = getBackgroundColor opts

getSlotCount [] = defaultSlotCount
getSlotCount ((OptionSlotCount s):_) = either (const $ error $ "invalid slot count: " ++ s) id $ parse slot_count "" s
getSlotCount (_:opts) = getSlotCount opts

autorotateOpts =
    [ GetOpt.Option ['f'] ["fuzz"]
                    (GetOpt.ReqArg OptionFuzz "DOUBLE")
                    "fuzz value passed to imagemagick (default: 10%)"
    , GetOpt.Option ['r'] ["resize-factor"]
                    (GetOpt.ReqArg OptionResizeFactor "INT%")
                    "image resize factor - adds speed (default: 33%)"
    , GetOpt.Option ['b'] ["bg","background"]
                    (GetOpt.ReqArg OptionBackgroundColor "<color>")
                    "background color (default: gray)"
    , GetOpt.Option ['s'] ["slots","slot-count"]
                    (GetOpt.ReqArg OptionSlotCount "INT")
                    "slots to use in search of rotation angle (default: 100)"
    , GetOpt.Option ['?','h'] ["help"]
                    (GetOpt.NoArg OptionHelp)
                    "show help message"
    ]

helpMessage = (GetOpt.usageInfo usageHeader autorotateOpts) ++ usageFooter

usageHeader = unlines
    [ ""
    , "autorotate [OPTIONS] in-file out-file"
    , ""
    ]

usageFooter = unlines
    [ ""
    , ""
    ]

percent = char '%' :: Parsec String u Char
integer = many1 digit >>= return . read
int = many1 digit >>= return . read
percent_param = (integer >>= return . ( % 100)) <* option '%' percent <* eof

fuzz_param = percent_param
resize_param = percent_param
slot_count = int

