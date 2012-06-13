module Control.Pipe.Utils
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe


toList :: Frame a b IO [a]
toList = Frame go where 
  go = do
    x <- await 
    case x of 
        Nothing -> close $ pure []
        Just a -> fmap (fmap (a:)) go

printer :: Show a => Frame a b IO ()
printer = Frame . forever $ awaitF >>= lift . print
