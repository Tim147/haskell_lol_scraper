module LOLType where

data Legends =
    Legends {legId :: Int, 
             legName :: String,
             legURL :: String,
             imgURL :: String,
             legDec :: String,
             imgPath :: String
            }
    deriving (Eq, Show, Read)

