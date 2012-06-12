{-# LANGUAGE TypeOperators, Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Filer.Flags 
  where

import Data.TypeLists
import Control.Applicative
import Data.Functor.Identity

import qualified System.Console.CmdArgs.Explicit as CmdArgs
import Data.Monoid


class ResolveFlag f n  where
    resolveflag :: f a -> n a

{-
class Resolve n l l' | l -> l' where
    resolve :: l -> n l'

instance Applicative n => Resolve n (HList '[]) (HList '[]) where
    resolve = pure

instance (Applicative n, ResolveFlag f n, Resolve n (HList l) (HList l')) => 
        Resolve n (HList ((f a) ': l)) (HList (a ': l'))  where
    resolve (HCons x xs) = HCons <$> (resolveflag x) <*> (resolve xs)
-}

data Proxy t = Proxy
proxy :: a -> Proxy a
proxy _ = Proxy

type family Unwrap a :: [*]
type instance Unwrap (HList l) = l

class Initial a where
    type Annotate a :: *
    initial :: Proxy a -> (Annotate a) 

data Default a = Default a | Selected a
  deriving Show

instance Initial Bool where
    type Annotate Bool = Default Bool
    initial _ = Default True

instance Initial Int where
    type Annotate Int = Default Int
    initial _ = Default 0


type family AnnotateList (ann :: [*]) :: [*]
type instance AnnotateList '[] = '[]
type instance AnnotateList (a ': l) = (Annotate a ': AnnotateList l)

data LProxy (a :: [*]) = LProxy
class InitialList (a :: [*]) where
    ilist :: LProxy a -> HList (AnnotateList a)

instance InitialList '[] where
    ilist _ = HNil

instance (Initial a, InitialList l) => InitialList (a ': l) where
    ilist _ = HCons (initial (Proxy :: Proxy a)) (ilist (LProxy :: LProxy l))

instance InitialList l => Initial (HList l) where
    type Annotate (HList l) = HList (AnnotateList l)
    initial _ = ilist (LProxy :: LProxy l)
    



{-
class InitialList (a :: *) where
    type Ann a :: [*]
    ilist :: Proxy a -> HList (Ann a)

instance InitialList (HList '[]) where
    type Ann (HList '[]) = '[]
    ilist _ = HNil

instance (Initial a, NotElem a (HList l), InitialList (HList l)) => InitialList (HList (a ': l)) where
    type Ann (HList (a ': l)) = (Annotation a) ': Ann (HList l)
    ilist _ = HCons (initial (Proxy :: Proxy a)) (ilist (Proxy :: Proxy (HList l)))
-}





{-
class DefaultFlag a where
    def :: Annotate a
    
class DefaultList a b | b -> a, a -> b where
    listdef :: Proxy a -> b


instance DefaultList (HList '[]) (HList '[]) where
    listdef _ = HNil

instance (Default a b , DefaultList (HList l) (HList l')) => 
        DefaultList (HList (a ': l)) (HList (b ': l')) where
    listdef _ = HCons (def (Proxy :: Proxy a)) (listdef (Proxy :: Proxy (HList l)))

class Flag a b where
    flags :: Proxy a -> CmdArgs.Group (CmdArgs.Flag b)

instance Flag (HList '[]) a where
    flags _ = CmdArgs.Group [] [] []

instance (Flag a b, Flag (HList l) b) => Flag (HList (a ': l)) b where
    flags _ = flags (Proxy :: Proxy a) <> flags (Proxy :: Proxy (HList l))
-}






{-



flag f _ (Default a) = f a
flag _ g (ReturnF a) = g a

unflag (Default a) = a
unflag (ReturnF a) = a
data Required a = Required String | ReturnR a
    deriving Show
required f _ (Required name) = f name
required _ g (ReturnR a) = g a
instance Default Bool (Flagy Bool) where def _ = Default True
instance Default Int (Required Int) where def _ = Required "Int"

instance ResolveFlag Flagy (Either String) where
    resolveflag = Right . unflag

instance ResolveFlag Required (Either String) where
    resolveflag (Required name) = Left $ "missing " ++ name
    resolveflag (ReturnR a) = Right a

    
type Opts = HList (Int ': Bool ': '[])
instance Modify (Flagy Bool) l => Flag (Flagy Bool) l where
    flags _ = CmdArgs.toGroup [CmdArgs.flagNone ["bool"] upd "flag bool"]
        where upd  = modify $ ReturnF . flag not not
instance Modify (Required Int) l => Flag (Required Int) l where
    flags _ = CmdArgs.toGroup 
        [CmdArgs.flagNone ["int"] (modify upd) "int flag"]
            where 
                upd :: Required Int -> Required Int
                upd = ReturnR . required (const 0) succ
  




-}
