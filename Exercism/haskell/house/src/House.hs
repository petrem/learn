module House (rhyme) where

import Data.List (inits, intersperse)


-- data ThingThatDid = ThingThatDid String String
--                   | LastThingThatDid String String

-- instance Show ThingThatDid where
--   show (ThingThatDid thing did) = "the " ++ thing ++ "\nthat " ++ did
--   show (LastThingThatDid thing did) = "the " ++ thing ++ " that " ++ did ++ ".\n"

-- allThingsThatDid :: [ThingThatDid]
-- allThingsThatDid = [LastThingThatDid "house" "Jack built"
--                    ,ThingThatDid "malt" "lay in"
--                    ,ThingThatDid "rat" "ate"
--                    ,ThingThatDid "cat" "killed"
--                    ,ThingThatDid "dog" "worried"
--                    ,ThingThatDid "cow with the crumpled horn" "tossed"
--                    ,ThingThatDid "maiden all forlorn" "milked"
--                    ,ThingThatDid "man all tattered and torn" "kissed"
--                    ,ThingThatDid "priest all shaven and shorn" "married"
--                    ,ThingThatDid "rooster that crowed in the morn" "woke"
--                    ,ThingThatDid "farmer sowing his corn" "kept"
--                    ,ThingThatDid "horse and the hound and the horn" "belonged to"
--                    ]

data ThingThatDid = ThingThatDid String (Maybe String)
                  -- | LastThingThatDid String String

instance Show ThingThatDid where
  show (ThingThatDid thing did) = "the " ++ thing ++ "\nthat " ++ did
  -- show (LastThingThatDid thing did) = "the " ++ thing ++ " that " ++ did ++ ".\n"

allThingsThatDid' :: [ThingThatDid]
allThingsThatDid' = [LastThingThatDid "house" "Jack built"
                   ,ThingThatDid "malt" "lay in"
                   ,ThingThatDid "rat" "ate"
                   ,ThingThatDid "cat" "killed"
                   ,ThingThatDid "dog" "worried"
                   ,ThingThatDid "cow with the crumpled horn" "tossed"
                   ,ThingThatDid "maiden all forlorn" "milked"
                   ,ThingThatDid "man all tattered and torn" "kissed"
                   ,ThingThatDid "priest all shaven and shorn" "married"
                   ,ThingThatDid "rooster that crowed in the morn" "woke"
                   ,ThingThatDid "farmer sowing his corn" "kept"
                   ,ThingThatDid "horse and the hound and the horn" "belonged to"
                   ]

rhyme :: String
rhyme = joinWith "\n" $ map (stanza . reverse) $ drop 1 . inits $ allThingsThatDid

stanza :: [ThingThatDid] -> String
stanza ts = "This is " ++ unwords (verses ts)

verses :: [ThingThatDid] -> [String]
verses = map show

-- TODO: function name is lacking... meaning
-- it maps f over the elements of a list but the last,
-- then appends g applied to the last
mapAndLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapAndLast f g = (++) <$> (map f) . init <*> (:[]) . g . last

joinWith :: String -> [String] -> String
joinWith s = concat . intersperse s
