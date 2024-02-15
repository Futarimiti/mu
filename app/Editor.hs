module Editor (Editor(..)) where

-- some programme able to edit a file
-- at a given filepath
newtype Editor = Editor { edit :: FilePath -> IO () }
