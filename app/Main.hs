module Main where

import Lib
import System.Exit
import Control.Exception

main :: IO ()
main = do

    anHero
    res <- try mainAction :: IO (Either SomeException ())
    case res of
         Left e -> die $ "Ошибка : " ++ show e
         Right () -> exitSuccess
