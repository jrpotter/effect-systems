{-# LANGUAGE ViewPatterns #-}

module Control.Monad.Free.Compose
  ( -- * State
    State (..),
    increment,
    runState,

    -- * Sum
    (:+:) (..),
    runTwoState,
    runState',
    threadedState,
    threadedState',

    -- * Member
    Member (..),
    Void,
    inject,
    project,
    get,
    put,
    run,
    threadedState'',
    threadedStateM'',

    -- * Exceptions
    Throw (..),
    throw,
    catch,
    runThrow,
    countDown,
    countDown',
  )
where

import Control.Monad.Free
import Data.Text (pack)
import qualified Text.Show as S
import Prelude hiding (State, Void, get, put, runState)

-- ========================================
-- State
-- ========================================

data State s k = Get (s -> k) | Put s k deriving (Functor)

instance (Show s, Show k) => Show (State s k) where
  show (Get _) = "Get <function>"
  show (Put s k) = "Put " <> S.show s <> " " <> S.show k

runState :: forall s a. s -> Free (State s) a -> (s, a)
runState s (Free (Get f)) = runState s (f s)
runState _ (Free (Put s' f)) = runState s' f
runState s (Pure a) = (s, a)

-- |
--
-- >>> runState 0 increment
-- (1, ())
increment :: Free (State Int) ()
increment = Free (Get (\s -> Free (Put (s + 1) (Pure ()))))

-- ========================================
-- Sum
-- ========================================

data (f :+: g) k = L (f k) | R (g k) deriving (Functor, Show)

infixr 4 :+:

runTwoState ::
  forall s1 s2 a.
  s1 ->
  s2 ->
  Free (State s1 :+: State s2) a ->
  (s1, s2, a)
runTwoState s1 s2 (Free (L (Get f))) = runTwoState s1 s2 (f s1)
runTwoState s1 s2 (Free (R (Get f))) = runTwoState s1 s2 (f s2)
runTwoState _ s2 (Free (L (Put s1 f))) = runTwoState s1 s2 f
runTwoState s1 _ (Free (R (Put s2 f))) = runTwoState s1 s2 f
runTwoState s1 s2 (Pure a) = (s1, s2, a)

runState' ::
  forall s a sig.
  Functor sig =>
  s ->
  Free (State s :+: sig) a ->
  Free sig (s, a)
runState' s (Pure a) = pure (s, a)
runState' s (Free (L (Get f))) = runState' s (f s)
runState' _ (Free (L (Put s f))) = runState' s f
runState' s (Free (R other)) = Free (fmap (runState' s) other)

{- ORMOLU_DISABLE -}

-- |
--
-- >>> runState "" (runState' 0 threadedState)
-- ("a",(1,()))
threadedState :: Free (State Int :+: State String) ()
threadedState =
  Free (L (Get (\s1 ->
    Free (R (Get (\s2 ->
      Free (L (Put (s1 + 1)
        (Free (R (Put (s2 ++ "a")
          (Pure ()))))))))))))

threadedState' :: Free (State String :+: State Int) ()
threadedState' =
  Free (R (Get (\s1 ->
    Free (L (Get (\s2 ->
      Free (R (Put (s1 + 1)
        (Free (L (Put (s2 ++ "a")
          (Pure ()))))))))))))

{- ORMOLU_ENABLE -}

-- ========================================
-- Membership
-- ========================================

class Member sub sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Member sig sig where
  inj = id
  prj = Just

instance
  {-# OVERLAPPABLE #-}
  Member sig (l1 :+: (l2 :+: r)) =>
  Member sig ((l1 :+: l2) :+: r)
  where
  inj sub = case inj sub of
    L l1 -> L (L l1)
    R (L l2) -> L (R l2)
    R (R r) -> R r
  prj sup = case sup of
    L (L l1) -> prj (L @l1 @(l2 :+: r) l1)
    L (R l2) -> prj (R @l1 @(l2 :+: r) (L @l2 l2))
    R r -> prj (R @l1 @(l2 :+: r) (R @l2 @r r))

instance {-# OVERLAPPABLE #-} Member sig (sig :+: r) where
  inj = L
  prj (L f) = Just f
  prj _ = Nothing

instance {-# OVERLAPPABLE #-} (Member sig r) => Member sig (l :+: r) where
  inj = R . inj
  prj (R g) = prj g
  prj _ = Nothing

data Void k deriving (Functor)

run :: forall a. Free Void a -> a
run (Pure a) = a
run _ = error (pack "impossible")

{- ORMOLU_DISABLE -}

threadedState'' ::
  Functor sig =>
  Member (State Int) sig =>
  Member (State String) sig =>
  Free sig ()
threadedState'' =
  Free (inj (Get @Int (\s1 ->
    Free (inj (Get (\s2 ->
      Free (inj (Put (s1 + 1)
        (Free (inj (Put (s2 ++ "a")
          (Pure ()))))))))))))

{- ORMOLU_ENABLE -}

inject ::
  forall a sub sup.
  Member sub sup =>
  sub (Free sup a) ->
  Free sup a
inject = Free . inj

project ::
  forall a sub sup.
  Member sub sup =>
  Free sup a ->
  Maybe (sub (Free sup a))
project (Free s) = prj s
project _ = Nothing

get :: forall s sig. Functor sig => Member (State s) sig => Free sig s
get = inject (Get pure)

put :: forall s sig. Functor sig => Member (State s) sig => s -> Free sig ()
put s = inject (Put s (pure ()))

threadedStateM'' ::
  Functor sig =>
  Member (State Int) sig =>
  Member (State String) sig =>
  Free sig ()
threadedStateM'' = do
  s1 <- get @Int
  s2 <- get @String
  put (s1 + 1)
  put (s2 ++ "a")
  pure ()

-- ========================================
-- Exceptions
-- ========================================

newtype Throw e k = Throw e deriving (Functor)

throw :: forall e a sig. Functor sig => Member (Throw e) sig => e -> Free sig a
throw e = inject (Throw e)

catch ::
  forall e a sig.
  Functor sig =>
  Free (Throw e :+: sig) a ->
  (e -> Free sig a) ->
  Free sig a
catch (Pure a) _ = pure a
catch (Free (L (Throw e))) h = h e
catch (Free (R other)) h = Free (fmap (`catch` h) other)

runThrow ::
  forall e a sig.
  Functor sig =>
  Free (Throw e :+: sig) a ->
  Free sig (Either e a)
runThrow (Pure a) = pure (Right a)
runThrow (Free (L (Throw e))) = pure (Left e)
runThrow (Free (R other)) = Free (fmap runThrow other)

countDown ::
  forall sig.
  Functor sig =>
  Member (State Int) sig =>
  Member (Throw ()) sig =>
  Free sig ()
countDown = do
  decr
  catch (decr >> decr) pure
  where
    decr ::
      forall sig2.
      Functor sig2 =>
      Member (State Int) sig2 =>
      Member (Throw ()) sig2 =>
      Free sig2 ()
    decr = do
      x <- get @Int
      if x > 0 then put (x - 1) else throw ()

{- ORMOLU_DISABLE -}

countDown' ::
  Functor sig =>
  Member (State Int) sig =>
  Member (Throw ()) sig =>
  Free sig ()
countDown' =
  Free (inj (Get @Int (\x ->
    let a = \k -> if x > 0 then Free (inj (Put (x - 1) k)) else throw ()
     in a (catch (Free (inj (Get @Int (\y ->
      let b = \k -> if y > 0 then Free (inj (Put (y - 1) k)) else throw ()
       in b (Free (inj (Get @Int (\z ->
        let c = \k -> if z > 0 then Free (inj (Put (z - 1) k)) else throw ()
         in c (Pure ()))))))))) pure))))

{- ORMOLU_ENABLE -}
