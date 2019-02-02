data Gender = Drake | Han deriving (Show, Ord, Eq, Bounded)

data Duck g = Mallard g | DomesticDuck g | Scaup g | Harlequin g

instance Show g => Show (Duck g) where
  show (Mallard g) = "Mallard " ++ show g
  show (DomesticDuck g) = "Domestic " ++ show g
  show (Scaup g) = "Scaup diving " ++ show g
  show (Harlequin g) = "Harlequin small sea " ++ show g

class Vocalizing a where
  quack :: a -> String

instance Vocalizing (Duck ) where
  quack (Mallard Drake) = "(manly) quack"
  quack (Mallard Han) = "quack"
  quack (Scaup _) = "scuap-scuap"
  quack (DomesticDuck _) = "quack"
  quack (Harlequin _) = "kee-kee-kee"


allTheDucks = [duck gender | duck <- [Mallard, Harlequin], gender <- [Drake, Han]]

describeQuack :: (Show a) => a -> String
describeQuack duck = "I'm a " ++ show duck ++ ". I go " ++ quack duck

-- main = do
--   fmap quack ducks
