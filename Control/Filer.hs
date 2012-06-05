module Control.Filer
  ( move )
  where

import System.FilePath (takeExtension, addExtension)
import System.IO (stdout, hFlush)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe

data Action = Move {originalname :: FilePath, newname :: FilePath}
  deriving Show

files :: FilePath -> Frame () FilePath IO ()
files _ = Frame . close $ mapM_ yieldF  ["a.ogg", "b.pdf", "c.mp3"]

filterP :: Monad m => (FilePath -> Bool) -> Frame FilePath FilePath m r 
filterP f = Frame . forever $ awaitF >>= \path -> when (f path) $ yieldF path

isAudioFile filename = takeExtension filename `elem` [".ogg", ".mp3"]

fixname :: Frame FilePath (FilePath, FilePath) IO r
fixname = Frame . forever $ do
    name <-awaitF 
    yieldF (name, addExtension "move" $ takeExtension name)


prompt :: IO Bool
prompt = do
    putStr "Should I move this file? [Yn], or ? for more options: "
    hFlush stdout
    char <- getChar <* putChar '\n'
    case char of 
        '\n' -> return True
        'y' -> return True
        ' ' -> return True
        'n' -> return False
        '?' -> putStrLn "Help" >> prompt
        otherwise -> putStrLn "Invalid response" >> prompt

promptP :: Frame (FilePath, FilePath) Action IO r
promptP = Frame . forever $ do 
    (filename, newname) <- awaitF
      
    lift $ putStrLn filename
    
    lift . putStrLn  $ " * " ++ newname

    doYield <- lift prompt 
    when doYield . yieldF $ Move {originalname=filename, newname=newname}

move :: FilePath -> Frame () Action IO ()
move dir = promptP <-< fixname <-< filterP isAudioFile <-< files dir

