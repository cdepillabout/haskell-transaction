{-# LANGUAGE DeriveFunctor #-}
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
  , toList
  , tMap
  , tFilter
  , tFilterMap

  -- * Types
  , Transaction
  , TransactionM
  ) where

import Control.Monad.Free

{- ==============
 -     Types
 - ============== -}

newtype Tuple a b = Tuple (a,b) deriving Functor

newtype TransactionM a x = TransactionM
  { unTransactionM :: Free (Tuple a) x
  } deriving (Functor, Applicative, Monad)

type Transaction a = TransactionM a ()

tranVal :: a -> x -> TransactionM a x
tranVal a x = TransactionM $ Free (Tuple (a, pure x))

{- ==============
 -   Operators
 - ============== -}

{- |
>>> :{
toList $ do
  action 4
  action 5
  action 6
:}
[4,5,6]
-}
action :: a -> Transaction a
action a = tranVal a ()

{- ==============
 -   Converters
 - ============== -}

{- |
>>> :{
toList $ do
  action 4
  tMap (+1) $ do
    action 5
    action 6
  action 7
:}
[4,6,7,7]
-}
tMap :: (a -> b) -> Transaction a -> Transaction b
tMap f = tFilterMap (pure . f)

{- |
>>> :{
toList $ do
  action 4
  tFilter even $ do
    action 5
    action 6
  action 7
:}
[4,6,7]
-}
tFilter :: (a -> Bool) -> Transaction a -> Transaction a
tFilter p = tFilterMap $ \a ->
  if p a
    then Just a
    else Nothing

{- |
>>> :{
toList $ do
  action 4
  tFilterMap (\x -> if even x then Just (x + 1) else Nothing) $ do
    action 5
    action 6
  action 7
:}
[4,7,7]
-}
tFilterMap :: forall a b. (a -> Maybe b) -> Transaction a -> Transaction b
tFilterMap f (TransactionM free) = TransactionM $ go free
  where
    go :: Free (Tuple a) () -> Free (Tuple b) ()
    go (Free (Tuple (a, next))) =
      case f a of
        Just b -> Free (Tuple (b, go next))
        Nothing -> go next
    go (Pure x) = Pure x

reduce :: forall b a . (b -> a -> b) -> b -> Transaction a -> b
reduce f b (TransactionM free) = go b free
  where
    go :: b -> (Free (Tuple a) ()) -> b
    go b (Free (Tuple (a, next))) = go (f b a) next
    go b (Pure x) = b
-- reduce f b (TVal a next) = undefined -- reduce f (f b a) next
-- reduce _ b (TNull ()) = undefined -- b

toList :: Transaction a -> [a]
toList trans = reduce (\f a -> f . (a:)) id trans []
