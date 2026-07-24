module Main.Path.Relative
  ( maybeParent
  , resolveAgainst
  , relativeFile
  ) where

import Path 
import System.FilePath ( splitDirectories, joinPath )

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
