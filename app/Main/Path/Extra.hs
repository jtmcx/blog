module Main.Path.Extra
  ( dot
  , root
  , (-<.>)
  , (-<.>!)
  , (</$>)
  , (<$/>)
  , maybeParent
  , resolveAgainst
  , relativeFile
  ) where

import Main.Path 
import System.FilePath ( splitDirectories, joinPath )
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

-- | The root directory @/@.
root :: Path Abs Dir
root = [absdir|/|]

-- | The current directory @.@.
dot :: Path Rel Dir
dot = [reldir|.|]

-- | Replace the extension for a given file.
(-<.>) :: MonadFail m => Path a File -> String -> m (Path a File)
(-<.>) = flip replaceExtension

-- | Replace the extension for a given file. Abort on error.
(-<.>!) :: Path a File -> String -> Path a File
p -<.>! ext = fromMaybe abort (p -<.> ext)
  where
    abort :: HasCallStack => a
    abort = error $ "internal error: failed to replace extension in " 
      ++ "path " ++ toFilePath p ++ " with " ++ ext

-- | Join two paths @p </$ q@, where 'q' might fail.
(</$>) :: MonadFail m => Path a Dir -> m (Path Rel t) -> m (Path a t)
(</$>) p q = (p </>) <$> q

-- | Join two paths @p $/> q@, where 'p' might fail.
(<$/>) :: MonadFail m => m (Path a Dir) -> Path Rel t -> m (Path a t)
(<$/>) p q = (</> q) <$> p

-- | Get the parent of a given path. Fail if we're at the root.
maybeParent :: Path a t -> Maybe (Path a Dir)
maybeParent path 
  | toFilePath path == toFilePath (parent path) = Nothing
  | otherwise = Just (parent path)

-- | Resolve a relative path to a file against a given base.
resolveAgainst :: Path a Dir -> FilePath -> Maybe (Path a File)
resolveAgainst base path = go base (splitDirectories path)
  where
    go :: Path a Dir -> [FilePath] -> Maybe (Path a File)
    go dir = \case
      []          -> fail "cannot resolve an empty path"
      ("/" : _)   -> fail "cannot resolve an absolute path"
      ("." : xs)  -> go dir xs
      (".." : xs) -> do p <- maybeParent dir; go p xs
      [x]         -> do f <- parseRelFile x; pure (dir </> f)
      (x : xs)    -> do d <- parseRelDir x; go (dir </> d) xs

-- | Calculate the relative path to a file from a given base directory.
relativeFile :: Path a Dir -> Path a File -> FilePath
relativeFile base target =
  let
    xs = splitDirectories (toFilePath base)
    ys = splitDirectories (toFilePath target)
    -- The number of common prefixed elements.
    common = length $ takeWhile id $ zipWith (==) xs ys
    -- Sequence of ups to get to the common parent.
    ups = replicate (length xs - common) ".."
    -- Sequence of downs from common parent to target.
    downs = drop common ys
  in joinPath (ups ++ downs)
