module Main where

import Data.Foldable (traverse_)
import Data.Time

data MonthAndDay = MonthAndDay Int Int deriving Eq

main :: IO ()
main = do
 now <- getCurrentTime
 traverse_ putStrLn (congratulate now mibEvents)

congratulate :: UTCTime -> [(MonthAndDay, String)] -> [String]
congratulate d events = map snd $ filter ((getMonthAndDay d ==) . fst) events


getMonthAndDay :: UTCTime -> MonthAndDay
getMonthAndDay = secondAndThird . toGregorian . utctDay
  where secondAndThird (_, x, y) = MonthAndDay x y

mibEvents :: [(MonthAndDay, String)]
mibEvents = [ (MonthAndDay 9 28, "Happy Birthday!")
            , (MonthAndDay 11 8, "Wishing you a very happy name day!")
            ]
