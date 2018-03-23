{-# LANGUAGE DeriveFunctor #-}
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

import Control.Monad.Free
import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable

{- ==============
 -     Types
 - ============== -}

newtype Tuple a b = Tuple (a,b) deriving (Bifunctor, Functor)

newtype TransactionM a x = TransactionM
  { unTransactionM :: Free (Tuple a) x
  } deriving (Functor, Applicative, Monad)

type Transaction a = TransactionM a ()

tranVal :: a -> x -> TransactionM a x
tranVal a x = TransactionM $ Free (Tuple (a, pure x))

instance Bifunctor TransactionM where
  first :: forall a b x. (a -> b) -> TransactionM a x -> TransactionM b x
  first f (TransactionM free) = TransactionM $ go free
    where
      go :: Free (Tuple a) x -> Free (Tuple b) x
      go (Free (Tuple (a, next))) = Free (Tuple (f a, go next))
      go (Pure x) = Pure x

  second = fmap

instance Bifoldable TransactionM where
  bifoldMap ::
       forall m a x. Monoid m
    => (a -> m)
    -> (x -> m)
    -> TransactionM a x
    -> m
  bifoldMap f g (TransactionM free) = go free
    where
      go :: Free (Tuple a) x -> m
      go (Free (Tuple (a, next))) = f a <> go next
      go (Pure x) = g x

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
action :: Monoid m => a -> TransactionM a m
action a = tranVal a mempty

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
tMap :: (a -> b) -> TransactionM a () -> TransactionM b ()
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
tFilter :: (a -> Bool) -> TransactionM a () -> TransactionM a ()
tFilter p = tFilterMap $ \a ->
  if p a
    then Just a
    else Nothing

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
tFilterMap :: forall a b. (a -> Maybe b) -> TransactionM a () -> TransactionM b ()
tFilterMap f (TransactionM free) = TransactionM $ go free
  where
    go :: Free (Tuple a) () -> Free (Tuple b) ()
    go (Free (Tuple (a, next))) =
      case f a of
        Just b -> Free (Tuple (b, go next))
        Nothing -> go next
    go (Pure x) = Pure x

reduce :: forall b a . (b -> a -> b) -> b -> TransactionM a () -> b
reduce f b = bifoldl' f const b

transToList :: TransactionM a () -> [a]
transToList = bifoldMap pure (const [])
