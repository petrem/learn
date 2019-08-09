module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM transcribe
  where
    transcribe 'G' = Right 'C'
    transcribe 'C' = Right 'G'
    transcribe 'T' = Right 'A'
    transcribe 'A' = Right 'U'
    transcribe x   = Left x
