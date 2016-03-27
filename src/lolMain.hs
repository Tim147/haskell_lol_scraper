module Main where

import LOLParser
import LOLDB
import LOLType
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)
import Data.Char


getLegend i =
    do dbh <- connect "lolt.db" 
       leg<-getLegends dbh i

       putStr $ show(leg)