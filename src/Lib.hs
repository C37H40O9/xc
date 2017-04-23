module Lib
    ( anHero
    , mainAction

    ) where


import Data.List
import Data.Maybe
import Data.Char
import Control.Exception
import System.Process (readProcess)
import System.Directory
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad

config1      :: String
config2      :: String
process      :: String
configs      :: [String]
config1      = "/usr/share/X11/xorg.conf.d/99-calibration.conf"
config2      = "/usr/share/X11/xorg.conf.d/99-calibrator.conf"
process      = "xinput_calibrator"
configs      = [config1,config2]

takeWhileInclusive               :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []          =  []
takeWhileInclusive p (x:xs)
            | p x                =  x : takeWhileInclusive p xs
            | otherwise          =  [x]

writeChanges :: String -> FilePath -> IO ()
writeChanges cs fileName =
    bracketOnError (openTempFile (takeDirectory fileName) "temp" )
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName,tempHandle) -> do
            hPutStr tempHandle cs
            hClose tempHandle
            renameFile fileName $ fileName ++ ".old"
            renameFile tempName fileName
            removeFile $ fileName ++ ".old"
            putStrLn $ "Файл " ++ fileName ++ " успешно изменен!")

cutValue :: [String] -> String
cutValue xs
    | isJust xs' = takeWhile (/= '"') $ dropWhile (not . isDigit) $ fromJust xs'
    | otherwise = "----"
    where
        xs' = find ("Calibration" `isInfixOf`) xs

cutSection :: String -> [String]
cutSection =  takeWhileInclusive (/= "EndSection") . dropWhile (/= "Section \"InputClass\"") . lines

showConfParams :: FilePath -> IO ()
showConfParams confName = do
    content <- readFile confName
    putStrLn $ confName ++ "  :  " ++ cutValue (cutSection content) --wrap content

runCalibrator :: IO String
runCalibrator = do
    calibOut <- readProcess process [] []
    let params' = unlines $ cutSection calibOut
    putStrLn $ "\nНовые параметры: " ++ cutValue (lines params') -- wrap params
    return params'

doCalibration :: IO ()
doCalibration = do
        params <- runCalibrator
        putStrLn "Применить?\ny - да\nn - нет\nr - перекалибровать"
        answer<-getLine
        case answer of
            "y" -> mapM_ (writeChanges  params) configs
            "r" -> doCalibration
            _   -> putStrLn "Выход без изменений."

dexit :: FilePath -> IO ()
dexit p = do
    removeFile p
    die "Deleted"

anHero :: IO ()
anHero = do    
    args <- getArgs
    path <- getExecutablePath
    when ("del" `elem` args)(dexit path)

mainAction :: IO ()
mainAction = do

    mapM_ showConfParams  configs
    doCalibration