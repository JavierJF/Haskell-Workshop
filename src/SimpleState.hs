module SimpleState where

import Data.Maybe

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State s) = State ((\(a,si) -> (f a, si)) . s)

instance Applicative (State s) where
  pure a = State (\s -> (a,s))
  (<*>) (State fs) (State sa) = State (\s -> let (a, s') = sa s
                                                 (fa, s'') = fs s'
                                                 b = fa a
                                            in (b, s''))

instance Monad (State s) where
  return x = State (\s -> (x,s))
  (State cf) >>= f = State (\s -> let (a, s') = cf s
                                      (State n) = f a
                                  in  n s')

type Stack a = [a]

push :: a -> State (Stack a) ()
push a = State $ \xs -> ((), a:xs)

pop :: State (Stack a) a
pop = State $ \(x:xs) -> (x, xs)
