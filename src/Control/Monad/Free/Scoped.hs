{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Free.Scoped
  ( -- * Free
    HFunctor (..),
    Syntax (..),
    Free (..),

    -- * Sum
    (:+:) (..),

    -- * Members
    Member (..),
    inject,
    project,

    -- * Lifting
    Lift (..),
    HState,
    hIncrement,
    runState,
    get,
    put,
    HVoid,
    run,

    -- * Exceptions
    Error (..),
    throw,
    catch,
    runError,
    countDown,
  )
where

import Control.Monad (ap)
import Control.Monad.Free.Compose (State (..), Void)
import Data.Text (pack)
import Prelude hiding (State, Void, get, put, runState)

{-# ANN module "HLINT: ignore Use <&>" #-}

-- ========================================
-- Free
-- ========================================

class HFunctor f where
  hmap ::
    (Functor m, Functor n) =>
    (forall x. m x -> n x) ->
    (forall x. f m x -> f n x)

class HFunctor f => Syntax f where
  emap :: (m a -> m b) -> (f m a -> f m b)

  weave ::
    (Monad m, Monad n, Functor ctx) =>
    ctx () ->
    Handler ctx m n ->
    (f m a -> f n (ctx a))

type Handler ctx m n = forall x. ctx (m x) -> n (ctx x)

data Free f a = Pure a | Free (f (Free f) a)

instance Syntax f => Functor (Free f) where
  fmap f m = m >>= pure . f

instance Syntax f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Syntax f => Monad (Free f) where
  Pure a >>= g = g a
  Free f >>= g = Free (emap (>>= g) f)

-- ========================================
-- Sum
-- ========================================

data (f :+: g) (m :: Type -> Type) a = L (f m a) | R (g m a)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
  hmap t (L f) = L (hmap t f)
  hmap t (R g) = R (hmap t g)

instance (Syntax f, Syntax g) => Syntax (f :+: g) where
  emap t (L f) = L (emap t f)
  emap t (R g) = R (emap t g)

  weave ctx hdl (L f) = L (weave ctx hdl f)
  weave ctx hdl (R g) = R (weave ctx hdl g)

-- ========================================
-- Members
-- ========================================

class (Syntax sub, Syntax sup) => Member sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance (Syntax sig) => Member sig sig where
  inj = id
  prj = Just

instance
  {-# OVERLAPPABLE #-}
  ( Syntax sig,
    Syntax l1,
    Syntax l2,
    Syntax r,
    Member sig (l1 :+: (l2 :+: r))
  ) =>
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

instance
  {-# OVERLAPPABLE #-}
  (Syntax sig, Syntax r) =>
  Member sig (sig :+: r)
  where
  inj = L
  prj (L f) = Just f
  prj _ = Nothing

instance
  {-# OVERLAPPABLE #-}
  (Member sig r, Syntax l) =>
  Member sig (l :+: r)
  where
  inj = R . inj
  prj (R g) = prj g
  prj _ = Nothing

inject ::
  forall a sub sup.
  Member sub sup =>
  sub (Free sup) a ->
  Free sup a
inject = Free . inj

project ::
  forall a sub sup.
  Member sub sup =>
  Free sup a ->
  Maybe (sub (Free sup) a)
project (Free s) = prj s
project _ = Nothing

-- ========================================
-- Lift
-- ========================================

newtype Lift sig (m :: Type -> Type) a = Lift (sig (m a))

type HState s = Lift (State s)

hIncrement :: Free (Lift (State Int)) ()
hIncrement = Free (Lift (Get (\s -> Free (Lift (Put (s + 1) (Pure ()))))))

instance Functor sig => HFunctor (Lift sig) where
  hmap t (Lift f) = Lift (fmap t f)

instance Functor sig => Syntax (Lift sig) where
  emap t (Lift f) = Lift (fmap t f)

  weave ctx hdl (Lift f) = Lift (fmap (\p -> hdl (fmap (const p) ctx)) f)

runState ::
  forall s a sig.
  Syntax sig =>
  s ->
  Free (HState s :+: sig) a ->
  Free sig (s, a)
runState s (Pure a) = pure (s, a)
runState s (Free (L (Lift (Get f)))) = runState s (f s)
runState _ (Free (L (Lift (Put s f)))) = runState s f
runState s (Free (R other)) = Free (weave (s, ()) hdl other)
  where
    hdl :: forall x. (s, Free (HState s :+: sig) x) -> Free sig (s, x)
    hdl = uncurry runState

get :: forall s sig. HFunctor sig => Member (HState s) sig => Free sig s
get = inject (Lift (Get Pure))

put :: forall s sig. HFunctor sig => Member (HState s) sig => s -> Free sig ()
put s = inject (Lift (Put s (pure ())))

type HVoid = Lift Void

run :: Free HVoid a -> a
run (Pure a) = a
run _ = error (pack "impossible")

-- ========================================
-- Exceptions
-- ========================================

data Error e m a
  = Throw e
  | forall x. Catch (m x) (e -> m x) (x -> m a)

instance HFunctor (Error e) where
  hmap _ (Throw x) = Throw x
  hmap t (Catch p h k) = Catch (t p) (t . h) (t . k)

instance Syntax (Error e) where
  emap _ (Throw e) = Throw e
  emap f (Catch p h k) = Catch p h (f . k)

  weave _ _ (Throw x) = Throw x
  weave ctx hdl (Catch p h k) =
    Catch
      (hdl (fmap (const p) ctx))
      (\e -> hdl (fmap (const (h e)) ctx))
      (hdl . fmap k)

throw :: Member (Error e) sig => e -> Free sig a
throw e = inject (Throw e)

catch :: Member (Error e) sig => Free sig a -> (e -> Free sig a) -> Free sig a
catch p h = inject (Catch p h pure)

runError ::
  forall e a sig.
  Syntax sig =>
  Free (Error e :+: sig) a ->
  Free sig (Either e a)
runError (Pure a) = pure (Right a)
runError (Free (L (Throw e))) = pure (Left e)
runError (Free (L (Catch p h k))) =
  runError p >>= \case
    Left e ->
      runError (h e) >>= \case
        Left e' -> pure (Left e')
        Right a -> runError (k a)
    Right a -> runError (k a)
runError (Free (R other)) =
  Free $ weave (Right ()) (either (pure . Left) runError) other

countDown ::
  forall sig.
  Syntax sig =>
  Member (HState Int) sig =>
  Member (Error ()) sig =>
  Free sig ()
countDown = do
  decr {- 1 -}
  catch (decr {- 2 -} >> decr {- 3 -}) pure
  where
    decr = do
      x <- get @Int
      if x > 0 then put (x - 1) else throw ()
