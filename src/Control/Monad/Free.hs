{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Free
  ( -- * First pass
    NonEmptyList (..),
    onePlusTwo,
    onePlusTwo',
    onePlusTwo'',
    runNonEmptyList,

    -- * Second pass
    NonEmptyList' (..),
    twoPlusThree,
    runNonEmptyList',

    -- * Third pass
    NonEmptyList'' (..),
    threePlusFour,
    runNonEmptyList'',

    -- * Free Monad
    Free (..),

    -- * Teletype
    Teletype (..),
    read,
    write,
    readThenWrite,
    runReadThenWrite,
  )
where

import Prelude hiding (Last)

{-# ANN module "HLINT: ignore Use let" #-}

{-# ANN module "HLINT: ignore Use <$>" #-}

-- ========================================
-- First Pass
-- ========================================

-- |
--
-- >>> runIdentity onePlusTwo'
-- 3
onePlusTwo :: forall m. Monad m => m Int
onePlusTwo = do
  a <- pure 1
  b <- pure 2
  pure (a + b)

onePlusTwo' :: forall m. Monad m => m Int
onePlusTwo' = pure 1 >>= (\a -> pure 2 >>= (\b -> pure (a + b)))

data NonEmptyList a = Last a | Cons a (a -> NonEmptyList a)

onePlusTwo'' :: NonEmptyList Int
onePlusTwo'' = Cons 1 (\a -> Cons 2 (\b -> Last (a + b)))

-- |
--
-- >>> runNonEmptyList onePlusTwo''
-- 3
runNonEmptyList :: NonEmptyList Int -> Int
runNonEmptyList (Last a) = a
runNonEmptyList (Cons a f) = runNonEmptyList (f a)

-- ========================================
-- Second Pass
-- ========================================

data NonEmptyList' f a = Last' a | Cons' a (f (NonEmptyList' f a))

instance (Functor f) => Functor (NonEmptyList' f) where
  fmap f (Last' a) = Last' (f a)
  fmap f (Cons' a g) = Cons' (f a) (fmap (fmap f) g)

{- ORMOLU_DISABLE -}

twoPlusThree :: NonEmptyList' (Reader Int) Int
twoPlusThree =
  Cons' 2 (reader (\a ->
    Cons' 3 (reader (\b ->
      Last' (a + b)))))

{- ORMOLU_ENABLE -}

-- |
--
-- >>> runNonEmptyList' twoPlusThree
-- 5
runNonEmptyList' :: NonEmptyList' (Reader Int) Int -> Int
runNonEmptyList' (Last' a) = a
runNonEmptyList' (Cons' a f) = runNonEmptyList' (runReader f a)

-- ========================================
-- Third Pass
-- ========================================

data Container a m k = Container a (m k) deriving (Functor)

data NonEmptyList'' f a = Last'' a | Cons'' (f (NonEmptyList'' f a))
  deriving (Functor)

{- ORMOLU_DISABLE -}

threePlusFour :: NonEmptyList'' (Container Int (Reader Int)) Int
threePlusFour =
  Cons'' (Container 3 (reader (\a ->
    Cons'' (Container 4 (reader (\b ->
      Last'' (a + b)))))))

{- ORMOLU_ENABLE -}

-- |
--
-- >>> runNonEmptyList'' threePlusFour
-- 5
runNonEmptyList'' :: NonEmptyList'' (Container Int (Reader Int)) Int -> Int
runNonEmptyList'' (Last'' a) = a
runNonEmptyList'' (Cons'' (Container a f)) = runNonEmptyList'' (runReader f a)

instance (Functor f) => Applicative (NonEmptyList'' f)

-- Intentionally empty so the @Monad@ instance compiles.

instance (Functor f) => Monad (NonEmptyList'' f) where
  Last'' a >>= g = g a
  Cons'' f >>= g = Cons'' (fmap (>>= g) f)

-- ========================================
-- Free Monad
-- ========================================

-- | The traditional representation of a free monad.
data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show (f (Free f a)), Show a) => Show (Free f a)

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free g) = Free (fmap (fmap f) g)

instance (Functor f) => Applicative (Free f) where
  pure = Pure

  Pure f <*> g = fmap f g
  Free f <*> g = Free (fmap (<*> g) f)

instance (Functor f) => Monad (Free f) where
  Pure a >>= g = g a
  Free f >>= g = Free (fmap (>>= g) f)

-- ========================================
-- Teletype
-- ========================================

data Teletype k = Read k | Write String k deriving (Functor, Show)

read :: Free Teletype String
read = Free (Read (Pure "hello"))

write :: String -> Free Teletype ()
write s = Free (Write s (Pure ()))

readThenWrite :: Free Teletype ()
readThenWrite = do
  input <- read
  write input

runReadThenWrite :: Free Teletype () -> IO ()
runReadThenWrite (Free (Write s f)) = putStrLn s >> runReadThenWrite f
runReadThenWrite (Free (Read f)) = runReadThenWrite f
runReadThenWrite (Pure _) = pure ()
