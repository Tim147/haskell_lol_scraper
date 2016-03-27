--module Interface where

import Graphics.UI.WX hiding (Point)
import Graphics.UI.WXCore hiding (Point)
import LOLDB
import LOLType
import DownloadHtml

import Control.Monad
import Control.Monad.State hiding(get)
import Data.Traversable hiding (get, sequence, mapM)
import Control.Monad.State.Lazy hiding (get)
import System.Random
import Database.HDBC

main :: IO()
main = do start legScreen

legScreen :: IO() 
legScreen
	= do
		dbh <- connect "lolt.db"
		addpage dbh 0
		legends <- getLegends dbh
		vbitmap <- variable [value := Nothing]
		f <- frame [ text := "Ledgends View", clientSize := sz 700 420, resizeable := False]
		p <- panel f [on paint := onPaint vbitmap, fullRepaintOnResize := False]


		let buttons = makelegimgpage dbh p legends 0
		tiles <- sequence buttons
		b <- bitmapButton p [picture := "img/left.bmp", text := "Ok" ,position := pt 200 360, clientSize := sz 100 50, on command := repage dbh tiles legends (-1)]
		b <- bitmapButton p [picture := "img/right.bmp", text := "Ok" ,position := pt 390 360, clientSize := sz 100 50, on command := repage dbh tiles legends 1]
		openImage p vbitmap "img/back.bmp"
		set p [clientSize := sz 700 420]

repage dbh tiles legends i
		= do
			p <- getpage dbh 1
			let page = changepage (p + i)
			legTiles <- mapM (\x -> bitmapCreateFromFile x) $ map (\x -> imgPath x) (map (legends !!) [(page*8)..(page*8 + 7)])
			mapM_ (\x -> (updatetile legTiles tiles x)) [0..7]
			mapM (\x -> updatecommand dbh (map (legends !!) [(page*8)..(page*8 + 7)]) tiles x) [0..7]
			updatepage dbh 1 $changepage page

			
			
updatetile legTiles tiles i
			= do bitmapButtonSetBitmapLabel (tiles!!i) $legTiles!!i

updatecommand dbh legends  tiles i
			= do set (tiles!!i) [on command := detailinfo dbh (legends!!i)]



makeLegImgButton :: IConnection conn => conn -> Panel() -> Legends -> IO(BitmapButton())
makeLegImgButton dbh p legend = bitmapButton p [picture := imgPath $ legend, text := "Ok" ,
							position := pt (convrlid $legId legend) (convllid $legId legend),
							on command := detailinfo dbh legend]


makelegimgpage :: IConnection conn => conn -> Panel() -> [Legends] -> Int -> [IO(BitmapButton())]
makelegimgpage dbh p legends page = map (makeLegImgButton dbh p) $map (legends !!) [page*8..(page*8 + 7)]

detailinfo ::  IConnection conn => conn -> Legends -> IO()
detailinfo dbh legend
	= do
		vbitmap <- variable [value := Nothing]
		f <- frame [ text := legName $legend, clientSize := sz 400 600, resizeable := False]
		p <- panel f [on paint := onPaint vbitmap, fullRepaintOnResize := False]
		st <- staticText p [text := legName $ legend, position := pt 240 30, color := red, fontWeight := WeightBold, fontSize := 20]
		bitmapButton p [picture := imgPath $ legend, text := "Ok" ,
							position := pt 80 20]
		b <- bitmapButton p [picture := "img/homepage.png", position := pt 230 90, clientSize := sz 100 30, on command := openweb (legURL legend)]
		downinfo dbh legend
		txt <- getstory dbh (legId legend)
		text <- textCtrl p [enabled := True, position := pt 35 150, wrap := WrapLine, clientSize := sz 320 400, text := txt ]
		openImage p vbitmap "img/subpage.bmp"
		set p [clientSize := sz 400 600]

convrlid :: Int -> Int
convrlid a
	| (a `mod` 8) < 1 = 540
	| (a `mod` 8) < 5 = 30 + 170 * ((a `mod` 8) - 1)
	| (a `mod` 8) < 8 = 30 + 170 * ((a `mod` 8) - 5)
	| otherwise = 0

convllid :: Int -> Int
convllid a
	| (a `mod` 8) < 1 = 220
	| (a `mod` 8) < 5 = 50
	| (a `mod` 8) < 8 = 220
	| otherwise = 0

changepage :: Int -> Int
changepage page = (page `mod` 16)

onPaint :: Valued w => w (Maybe (Bitmap ())) -> DC a -> t -> IO ()
onPaint vbitmap dc viewArea
  = do mbBitmap <- get vbitmap value
       case mbBitmap of
         Nothing -> return () 
         Just bm -> drawBitmap dc bm pointZero False []

openImage :: (Paint w1, Valued w, Dimensions w1) => w1 -> w (Maybe (Bitmap ())) -> FilePath -> IO ()
openImage sw vbitmap fname
    = do
        bm <- bitmapCreateFromFile fname
        set vbitmap [value := Just bm]
        bmsize <- get bm size
        set sw [virtualSize := bmsize]
        repaint sw
