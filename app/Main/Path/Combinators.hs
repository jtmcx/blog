module Main.Path.Combinators
  ( (</$)
  , ($/>)
  , ($/$)
  , (-<.>)
  ) where

import Control.Monad.Catch (MonadThrow)
import Data.Text (Text, unpack)
import Path (Rel, Dir, Path, File, replaceExtension, (</>))

-- | Join two paths 'p </$ q', where 'q' might fail.
(</$) :: MonadThrow m => Path a Dir -> m (Path Rel t) -> m (Path a t)
(</$) p q = (p </>) <$> q

-- | Join two paths 'p $/> q', where 'p' might fail.
($/>) :: MonadThrow m => m (Path a Dir) -> Path Rel t -> m (Path a t)
($/>) p q = (</> q) <$> p

-- | Join two paths 'p $/$ q', where 'p' or 'q' might fail.
($/$) :: MonadThrow m => m (Path a Dir) -> m (Path Rel t) -> m (Path a t)
($/$) p q = (</>) <$> p <*> q

-- | Replace the extension of a given path.
-- This shadows an existing function in the @path@ module.
(-<.>) :: MonadThrow m => Path a File -> Text -> m (Path a File)
(-<.>) p ext = replaceExtension (unpack ext) p