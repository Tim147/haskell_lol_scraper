module LOLDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import LOLType
import Control.Monad(when)
import Data.List(sort)

connect :: FilePath -> IO Connection
connect fp =
    do dbh <- connectSqlite3 fp
       prepDB dbh
       return dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("LEGENDS" `elem` tables)) $
           do run dbh "CREATE TABLE LEGENDS (\
                       \legId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                      \legName TEXT UNIQUE,\
                       \legURL TEXT UNIQUE,\
                       \imgURL TEXT UNIQUE,\
                       \legDec TEXT,\
                       \imgPath TEXT UNIQUE)" []
              return ()
       when (not ("page" `elem` tables)) $
           do run dbh "CREATE TABLE page (\
                       \pid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \pagenum INTEGER)" []
              return ()
       commit dbh

addLegends :: IConnection conn => conn -> Legends -> IO ()
addLegends dbh legend = 
         run dbh "INSERT OR IGNORE INTO LEGENDS (legName, legURL, imgURL, legDec, imgPath) \
                \VALUES (?, ?, ?, ?, ?)"
                [toSql (legName legend), toSql (legURL legend), toSql (imgURL legend), toSql (legDec legend), toSql (imgPath legend)]
         >> return ()

updateLegends :: IConnection conn => conn -> Legends -> IO ()
updateLegends dbh legend =
    run dbh "UPDATE LEGENDS SET legDec = ? WHERE legName = ?" 
            [toSql (legDec legend), toSql (legName legend)]
    >> return ()

getLegend :: IConnection conn => conn -> Integer-> IO (Maybe Legends)
getLegend dbh i =
    do res <- quickQuery' dbh 
              "SELECT legURL, legName, imgURL, imgPath FROM LEGENDS WHERE legId = ?" [toSql i]
       putStr $ show(res)
       case res of
         [x] -> return (Just (convLegends x))
         [] -> return Nothing
         x -> fail $ "Really bad error; more than one Legends with ID"

getLegends :: IConnection conn => conn ->IO [Legends]
getLegends dbh =
    do res <- quickQuery' dbh 
              "SELECT legId, legURL, legName, imgURL, legDec, imgPath FROM LEGENDS ORDER BY legId" []
       return (map convLegends res)

convLegends :: [SqlValue] -> Legends
convLegends [svid, svlURL, svName, svURL, svDec, svPath] =
    Legends {legId = fromSql svid,
             legURL = fromSql svlURL,
             legName = fromSql svName,
             imgURL = fromSql svURL,
             legDec = fromSql svDec,
             imgPath = fromSql svPath}
convLegends x = error $ "Can't convert " ++ show x

getpage :: IConnection conn => conn -> Integer-> IO (Int)
getpage dbh i =
    do res <- quickQuery' dbh 
              "SELECT pid, pagenum FROM page WHERE pid = ?" [toSql i]
       return $ head (map convpage res)

convpage :: [SqlValue] -> Int
convpage [svid,svpage] = fromSql svpage
convpage x = error $ "Can't convert " ++ show x

updatepage :: IConnection conn => conn -> Int -> Int -> IO ()
updatepage dbh pid pagenum  =
    run dbh "UPDATE page SET pagenum = ? WHERE pid = ?" 
            [toSql pagenum, toSql pid]
    >> return ()

addpage :: IConnection conn => conn -> Int -> IO ()
addpage dbh pagenum = 
         run dbh "INSERT INTO page (pagenum) \
                \VALUES (?)"
                [toSql pagenum]
         >> return ()

getstory :: IConnection conn => conn -> Int-> IO (String)
getstory dbh i =
    do res <- quickQuery' dbh 
              "SELECT legId, legDec FROM LEGENDS WHERE legId = ?" [toSql i]
       return $ head (map convstory res)

convstory :: [SqlValue] -> String
convstory [svid,svpage] = fromSql svpage
convstory x = error $ "Can't convert " ++ show x