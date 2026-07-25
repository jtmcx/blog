-- | Re-export of Path using 'MonadFail' instead of 'MonadThrow'.
module Main.Path
  -- * Re-exported Definitions
  ( module Path
  -- * Functions Converted to MonadFail
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , parseSomeDir
  , parseSomeFile
  , replaceProperPrefix
  , stripProperPrefix
  , addExtension
  , replaceExtension
  , splitExtension
  , fileExtension
  ) where

import Path hiding
  ( (-<.>)
  , (<.>)
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , parseSomeDir
  , parseSomeFile
  , replaceProperPrefix
  , stripProperPrefix
  , addExtension
  , replaceExtension
  , splitExtension
  , fileExtension
  , addFileExtension -- deprecated
  , setFileExtension -- deprecated
  , stripDir -- deprecated
  )
import qualified Path as Path


-- Functions Converted from MonadThrow to MonadFail

failLeft :: (MonadFail m, Show e) => Either e a -> m a
failLeft = either (fail . show) pure

parseAbsDir :: MonadFail m => FilePath -> m (Path Abs Dir)
parseAbsDir = failLeft . Path.parseAbsDir

parseAbsFile :: MonadFail m => FilePath -> m (Path Abs File)
parseAbsFile = failLeft . Path.parseAbsFile

parseRelDir :: MonadFail m => FilePath -> m (Path Rel Dir)
parseRelDir = failLeft . Path.parseRelDir

parseRelFile :: MonadFail m => FilePath -> m (Path Rel File)
parseRelFile = failLeft . Path.parseRelFile

parseSomeDir :: MonadFail m => FilePath -> m (SomeBase Dir)
parseSomeDir = failLeft . Path.parseSomeDir

parseSomeFile :: MonadFail m => FilePath -> m (SomeBase File) 
parseSomeFile = failLeft . Path.parseSomeFile

stripProperPrefix :: MonadFail m => Path b Dir -> Path b t -> m (Path Rel t)
stripProperPrefix p = failLeft . Path.stripProperPrefix p

replaceProperPrefix :: MonadFail m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
replaceProperPrefix p p' = failLeft . Path.replaceProperPrefix p p'

splitExtension :: MonadFail m => Path b File -> m (Path b File, String)
splitExtension = failLeft . Path.splitExtension

replaceExtension :: MonadFail m => String -> Path b File -> m (Path b File)
replaceExtension s = failLeft . Path.replaceExtension s

addExtension :: MonadFail m => String -> Path b File -> m (Path b File) 
addExtension s = failLeft . Path.addExtension s

fileExtension :: MonadFail m => Path b File -> m String
fileExtension = failLeft . Path.fileExtension