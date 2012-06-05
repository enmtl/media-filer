module Main 
  where

import System.IO (stdin, hSetBuffering, BufferMode(..))

import Control.Applicative

import Control.Pipe
import qualified Control.Filer as Filer

toList :: Frame a b IO [a]
toList = Frame go where 
  go = do
    x <- await 
    case x of 
        Nothing -> close $ pure []
        Just a -> fmap (fmap (a:)) go

main = do 
    hSetBuffering stdin NoBuffering
    let discard x = [] <$ x
    files <- runFrame $ toList <-< discard (Filer.move ".")
    mapM_ print files
