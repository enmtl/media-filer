module Control.Filer.Search
  (findFiles)
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe

import Data.Maybe
import Data.Filer

import System.FilePath.Find hiding (find) 
import qualified System.FilePath.Find as Find


hidden :: FilterPredicate 
hidden = maybe False ((==) '.') . listToMaybe <$> Find.fileName

regular :: FilterPredicate
regular = fileType ==? RegularFile

audio :: AudioFormat -> FilterPredicate
audio formats = (`elem` listAudioFormat formats) <$> extension
    

find :: FilterPredicate -> [FilePath] -> Frame () FilePath IO ()
find query paths = Frame . close . forM_ paths $ \path -> do
                files <- lift $ Find.find (not <$> hidden) query path
                mapM_ yieldF files

findFiles :: AudioFormat -> [FilePath] -> Frame () FilePath IO ()
findFiles afmt = find (regular &&? audio afmt) 




            
    
