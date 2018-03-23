{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Data.Transaction

Copyright   :  Kadzuya Okamoto 2018
License     :  MIT

Stability   :  experimental
Portability :  unknown

Monadic representation of transactions.
-}
module Data.Transaction
  (
  -- * Constructors
  action

  -- * Converters
  , reduce
  , transToList
  , tMap
  , tFilter
  , tFilterMap

  -- * Types
  , Transaction
  , TransactionM(..)
  ) where

import Prelude hiding (filter)

import Control.Monad.Free
import Data.Bifoldable
import Data.Bifunctor
import Data.Witherable

{- ==============
 -     Types
 - ============== -}

newtype Flip f b a = Flip { unFlip :: f a b }

flipped :: (Flip f b a -> Flip g d c) -> f a b -> g c d
flipped f = unFlip . f . Flip

newtype Tuple a b = Tuple (a,b) deriving (Bifunctor, Functor)

combineTuple :: (a -> b -> c) -> Tuple a b -> c
combineTuple f (Tuple (a, b)) = f a b

newtype TransactionM a x = TransactionM
  { unTransM :: Free (Tuple a) x
  } deriving (Functor, Applicative, Monad)

type Transaction a = TransactionM a ()

freeTupVal :: a -> x -> Free (Tuple a) x
freeTupVal a x = Free (Tuple (a, pure x))

tranVal :: a -> x -> TransactionM a x
tranVal a x = TransactionM $ freeTupVal a x

instance Bifunctor TransactionM where
  first :: forall a b x. (a -> b) -> TransactionM a x -> TransactionM b x
  first f (TransactionM free) = TransactionM $ hoistFree (first f) free

  second :: forall a x y. (x -> y) -> TransactionM a x -> TransactionM a y
  second = fmap

instance Bifoldable TransactionM where
  bifoldMap
    :: forall m a x. Monoid m => (a -> m) -> (x -> m) -> TransactionM a x -> m
  bifoldMap f g = iter (combineTuple (<>)) . unTransM . bimap f g

instance Functor (Flip TransactionM x) where
  fmap :: (a -> b) -> Flip TransactionM x a -> Flip TransactionM x b
  fmap f (Flip trans) = Flip $ first f trans

instance Filterable (Flip TransactionM a) where
  catMaybes :: forall x. Flip TransactionM a (Maybe x) -> Flip TransactionM a x
  catMaybes (Flip (TransactionM free)) = Flip $ TransactionM $ foldFree go free
    where
      go :: Tuple (Maybe x) z -> Free (Tuple x) z
      go (Tuple (Nothing, z)) = pure z
      go (Tuple (Just x, z)) = freeTupVal x z

{- ==============
 -   Operators
 - ============== -}

{- |
>>> :{
transToList $ do
  action 4
  action 5
  action 6
:}
[4,5,6]
-}
action :: a -> TransactionM a ()
action a = tranVal a ()

{- ==============
 -   Converters
 - ============== -}

{- |
>>> :{
transToList $ do
  action 4
  tMap (+1) $ do
    action 5
    action 6
  action 7
:}
[4,6,7,7]
-}
tMap :: (a -> b) -> TransactionM a x -> TransactionM b x
tMap = first

{- |
>>> :{
transToList $ do
  action 4
  tFilter even $ do
    action 5
    action 6
  action 7
:}
[4,6,7]
-}
tFilter :: (a -> Bool) -> TransactionM a x -> TransactionM a x
tFilter p = flipped (filter p)

{- |
>>> :{
transToList $ do
  action 4
  tFilterMap (\x -> if even x then Just (x + 1) else Nothing) $ do
    action 5
    action 6
  action 7
:}
[4,7,7]
-}
tFilterMap :: forall a b x. (a -> Maybe b) -> TransactionM a x -> TransactionM b x
tFilterMap f = flipped (mapMaybe f)

reduce :: forall b a x. (b -> a -> b) -> b -> TransactionM a x -> b
reduce f b = bifoldl' f const b

transToList :: TransactionM a x -> [a]
transToList = bifoldMap pure (const [])
