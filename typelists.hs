{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances#-}
module Data.TypeLists 

  where


data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)

instance Show (HList '[]) where
    show HNil = "`[]"
instance (Show x, Show (HList l)) => Show (HList (x ': l)) where
    show (HCons e l) = "`[" ++ show e ++ case l of
                            HNil -> "]"
                            _ -> (',' :) .  drop 2 $ show l

(.*.) = HCons

infixr 2 .*.

class Elem e l where
    getElem :: l -> e

instance NotElem e (HList l) => Elem e (HList (e ': l)) where
    getElem (HCons e _) = e

instance Elem e (HList l) => Elem e (HList (e' ': l)) where
    getElem (HCons _ l) = getElem l


class NotElem e l
class FailureNotUnique e
instance NotElem e (HList '[])
instance FailureNotUnique e => NotElem e (HList (e ': l))
instance NotElem e (HList l) => NotElem e (HList (e' ': l))

class Modify e l where
    modify :: (e -> e) -> l -> l

instance NotElem e (HList l) => Modify e (HList (e ': l)) where
    modify f (HCons e l) = HCons (f e) l

instance (Modify e (HList l)) => Modify e (HList (e' ': l)) where
    modify f (HCons e l) = HCons e (modify f l)


