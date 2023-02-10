{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Error (Error(..), MonadTrace, mapError, trace) where

import Control.Monad.Trans.Accum
import Control.Monad.Trans.State

newtype Error a = Error (Either String a) deriving (Functor, Applicative, Monad)

mapError :: (String -> b) -> (a -> b) -> Error a -> b
mapError _ f (Error (Right x)) = f x
mapError f _ (Error (Left x)) = f x

instance MonadFail Error where
  fail = Error . Left

class MonadFail m => MonadTrace m where
  trace :: String -> m a -> m a

instance MonadTrace Error where
  trace s (Error (Left s')) = Error (Left (s' ++ s))
  trace _ x = x

instance (Monoid w, MonadTrace m) => MonadTrace (AccumT w m) where
  trace s = mapAccumT (trace s)

instance (MonadTrace m) => MonadTrace (StateT s m) where
  trace s = mapStateT (trace s)