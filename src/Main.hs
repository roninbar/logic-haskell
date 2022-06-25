{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- The state monad

type State = [Bool]

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (x,)

  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- Logic Gates

data Gate = In | Not Gate | And [Gate] | Or [Gate]

next :: ST Bool
next = S (\(b : bs) -> (b, bs))

evalA :: Gate -> ST Bool -> ST Bool
evalA In i = i
evalA (Not g) i = not <$> evalA g i
evalA (And gs) i = and <$> sequenceA [evalA g i | g <- gs]
evalA (Or gs) i = or <$> sequenceA [evalA g i | g <- gs]

network :: Gate
network = And [Or [In, Not In, In], Not (Or [In, In])]

main :: IO ()
main = print (app (evalA network next) [True, False, True, False, True])

