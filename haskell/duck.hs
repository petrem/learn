data Ducks = Mallard | DomesticDuck | Scaup | Harlequin

instance Show Ducks where
  show Mallard = "mallard duck"
  show DomesticDuck = "domestic duck"
  show Scaup = "scaup diving duck"
  show Harlequin = "harlequin small sea duck"

class Duck a where
  quack :: a -> String

instance Duck Ducks where
  quack Mallard = "quack"
  quack Scaup = "scuap-scuap"
  quack DomesticDuck = "quack"
  quack Harlequin = "kee-kee-kee"


ducks = [Mallard, Scaup, DomesticDuck, Harlequin]

describeQuack :: (Show a, Duck a) => a -> String
describeQuack duck = "I'm a " ++ show duck ++ ". I go " ++ quack duck

-- main = do
--   fmap quack ducks
