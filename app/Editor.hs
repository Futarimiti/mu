module Editor (Editor(..), shellEditor, vim, notepad) where

import           Control.Applicative       (Alternative (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           System.Environment        (lookupEnv)
import           System.Process            (callProcess)

-- some programme able to edit a file
-- at a given filepath
newtype Editor = Editor { edit :: FilePath -> IO () }

shellEditor :: MaybeT IO Editor
shellEditor = do cmd <- shellEditorCommand
                 return Editor { edit = callProcess cmd . pure }

-- | Try to get a shell editor from VISUAL & EDITOR
shellEditorCommand :: MaybeT IO FilePath
shellEditorCommand = MaybeT $ do visual <- lookupEnv "VISUAL"
                                 editor <- lookupEnv "EDITOR"
                                 return $ visual <|> editor

vim :: Editor
vim = Editor { edit = callProcess "vim" . pure }

notepad :: Editor
notepad = Editor { edit = callProcess "editor" . pure }
