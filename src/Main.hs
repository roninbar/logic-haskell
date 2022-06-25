{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- The state monad

newtype ST s a = S (s -> (a, s))

app :: ST s a -> s -> (a, s)
app (S st) = st

instance Functor (ST s) where
  fmap :: (a -> b) -> ST s a -> ST s b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative (ST s) where
  pure :: a -> ST s a
  pure x = S (x,)

  (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

instance Monad (ST s) where
  (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- Logic Gates

data Gate = In | Not Gate | And [Gate] | Or [Gate] deriving (Show)

next :: ST [Bool] Bool
next = S (\(b : bs) -> (b, bs))

evalA :: Gate -> ST [Bool] Bool
evalA In = next
evalA (Not g) = not <$> evalA g
evalA (And gs) = and <$> traverse evalA gs
evalA (Or gs) = or <$> traverse evalA gs

evalM :: Gate -> ST [Bool] Bool
evalM In = next
evalM (Not g) = do
  b <- evalM g
  return (not b)
evalM (And gs) = do
  bs <- mapM evalM gs
  return (and bs)
evalM (Or gs) = do
  bs <- mapM evalM gs
  return (or bs)

network :: Gate
network = Or [Not (And [In, Not (Or [In, In]), Not In]), In]

main :: IO ()
main = print (app (evalA network) [True, False, True, False, True])
