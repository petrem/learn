module utmp where

import qualified Data.ByteString.Lazy as BL


data UtmpExitStatus = UtmpExitStatus {}
data Utmp = Utmp {}

data UtmpRecord = UtmpRecord {
    exitStatus :: UtmpExitStatus
  , utmp :: Utmp

main :: IO ()
main = do
  contents <- BL.getContents


parseUtmpEntry :: BL.ByteString -> Maybe (UtmpEntry, BL.ByteString)
