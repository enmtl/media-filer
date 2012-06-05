module Control.Filer
  ( move )
  where

import System.FilePath (takeExtension, addExtension)
import System.IO (stdout, hFlush)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe hiding (Prompt)

data Action = Move {originalname :: FilePath, newname :: FilePath}
            | Copy {originalname :: FilePath, newname :: FilePath}
  deriving Show

data Prompt = PMove | PCopy | PSkip | PQuit

files :: FilePath -> Frame () FilePath IO ()
files _ = Frame . close $ mapM_ yieldF  ["a.ogg", "b.pdf", "c.mp3"]

filterP :: Monad m => (FilePath -> Bool) -> Frame FilePath FilePath m r 
filterP f = Frame . forever $ awaitF >>= \path -> when (f path) $ yieldF path

isAudioFile filename = takeExtension filename `elem` [".ogg", ".mp3"]

fixname :: Frame FilePath (FilePath, FilePath) IO r
fixname = Frame . forever $ do
    name <-awaitF 
    yieldF (name, addExtension "move" $ takeExtension name)


prompt :: IO Prompt
prompt = do
    putStr "What should I do with this file? [mcSq], or ? for more options: "
    hFlush stdout
    char <- getChar <* putChar '\n'
    let def = PSkip
    case char of 
        ' ' -> return def
        '\n' -> return def
        'm' -> return PMove
        'c' -> return PCopy
        's' -> return PSkip
        'q' -> return PQuit
        '?' -> putStrLn "Help" >> prompt
        _ -> putStrLn "Invalid response" >> prompt

promptP :: Frame (FilePath, FilePath) Action IO ()
promptP = Frame go
  where 
    go = do 
        (filename, newname) <- awaitF
          
        lift $ putStrLn filename
        
        lift . putStrLn  $ " * " ++ newname

        act <- lift prompt 
        case act of
            PMove -> yieldF $ Move {originalname=filename, newname=newname}
            PCopy -> yieldF $ Copy {originalname=filename, newname=newname}
            _ -> return ()
        case act of 
            PQuit -> close $ return () 
            _ -> go




move :: FilePath -> Frame () Action IO ()
move dir = promptP <-< fixname <-< filterP isAudioFile <-< files dir


