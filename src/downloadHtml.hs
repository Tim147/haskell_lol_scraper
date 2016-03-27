
module DownloadHtml where

import Text.HTML.TagSoup
import Network.HTTP

import Data.Char
import Data.List
import System.IO
import Network.URI
import System.Directory
import System.IO
import System.Cmd
import System.Environment
import Data.Maybe
import Database.HDBC
import Network.Socket(withSocketsDo)
import LOLType
import LOLDB

import Data.String.UTF8 as UTF8 hiding (take,length)



downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> 
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [Header HdrContentType "text/xml; charset=utf-8",Header HdrAcceptLanguage "zh-Hans-CN, zh-Hans; q=0.7, ko; q=0.3"]:: [Header],
                             rqBody = ""}
          uri = fromJust $ parseURI url

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

downinfo dbh legend =
	do
    src <- downloadURL (legURL legend)
    case src of
         Left x -> writeError dbh legend x
         Right doc -> downloadsyn dbh legend doc

writeError dbh legend x = 
	do 
		updateLegends dbh (legend {legDec = x})
		putStrLn $ show(x)

--test =
--	do
--    src <- downloadURL "http://lol.qq.com/web201310/info-defail.shtml?id=Brand"
--    hSetEncoding stdout utf8
--    case src of
--         Left x -> putStrLn x
--         Right doc ->putStrLn doc

--decdown =
--	do
--    src <- downloadURL "http://gameinfo.na.leagueoflegends.com/en/game-info/champions/aatrox/"
--    case src of
--         Left x -> putStrLn x
--         Right doc -> dectest doc

--dectest src = do 

--    let tags = dropWhile (~/= "<div class=section-wrapper id=champion-lore>") $ parseTags src
--    	t = [x|TagOpen "div" [("class","")]]
--    	text = ascii $ innerText $ dropWhile (~/= "<div class=default-1-2>") $ take 35 tags

--    putStrLn $ show(text)
--    where 
--          ascii ('\n':xs) = ascii xs
--          ascii ('\t':xs) = ascii xs
--          ascii (x:xs) = x : ascii xs
--          ascii [] = []	


downloadsyn dbh legend src = do

    let tags = dropWhile (~/= "<div class=section-wrapper id=champion-lore>") $ parseTags src
    	text = ascii $ innerText $ dropWhile (~/= "<div class=default-1-2>") $ take 35 tags

    updateLegends dbh (legend {legDec = text})
    where 
          ascii ('\n':xs) = ascii xs
          ascii ('\t':xs) = ascii xs
          ascii (x:xs) = x : ascii xs
          ascii [] = []	


parse = do 
    src <- readFile "temp.html"
    --dbh <- connect "loly.db"
    let tags =  filter isTagText $ parseTags src
    	text =  map (f tags) [0..127]
    	urltags = parseTags src
    	dref = [x |TagOpen "a" [("href",x)] <- urltags]
    	sref = map (dref!!) [0,2..254]
    	imghref = map ("http:"++) [x |TagOpen "img" [(_,_),(_,_),("data-original",x)] <- urltags]
    	--legends = [Legends {legId = 0, legName=a, legURL=b, imgURL=c, legDec="", imgPath = "img/"++a}| i<-[0..127], a=text!!i,b=sref!!i,c=imghref!!i]
    	legends = map (toLegend text sref imghref) [0..127]

    --mapM_ (addLegends dbh) legends
    --commit dbh
    mapM_ saveImg legends
    --mapM_ (down dbh) legends
    --commit dbh
    --disconnect dbh
    putStrLn $ show(length(imghref))
   where 
    	  f :: [Tag String] -> Int -> String
          f xs i = fromTagText (xs !! i)
          stringtolower::String -> String
          stringtolower = map toLower
          toLegend::[String]->[String]->[String]->Int->Legends
          toLegend names hrefs imgs i = 
          	Legends {legId = 0, legName=names!!i, legURL="http://gameinfo.na.leagueoflegends.com/en/game-info/champions/"++hrefs!!i, imgURL=imgs!!i, legDec="", imgPath = "img/"++(names!!i)++".png"}

saveImg :: Legends -> IO ()
saveImg legend =
    do resp <- downloadURL (imgURL legend)
       case resp of
         Left x -> do putStrLn x
         Right doc -> 
             do file <- openBinaryFile filename WriteMode
                hPutStr file doc
                hClose file
                putStrLn filename
          -- This function ought to apply an extension based on the filetype
    where filename = (imgPath legend)

smg :: IO ()
smg =
    do resp <- downloadURL "http://ddragon.leagueoflegends.com/cdn/5.23.1/img/champion/Aatrox.png"
       case resp of
         Left x -> do putStrLn x
         Right doc -> 
             do file <- openBinaryFile filename WriteMode
                hPutStr file doc
                hClose file
                putStrLn filename
          -- This function ought to apply an extension based on the filetype
    where filename = "test.png"

openweb url
  = do
    system $ "Chrome.exe " ++ url
    return ()



