-- Applicative Functors
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State

-- 1.1
data Tree a = Node a [Tree a]
instance Functor Tree where
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

-- 1.2
data Either' a b = Left' a | Right' b
instance Functor (Either' e) where
  fmap f (Right' b) = Right' (f b)
  fmap _ (Left' a) = Left' a

-- 2.2
instance Applicative (Either' e) where
  pure b = Right' b
  (Right' f) <*> (Right' b) = Right' (f b)
  (Left' a) <*> _ = Left' a
  _ <*> (Left' a) = Left' a

-- 3.2
liftA5' :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f b
liftA5' f a b c d e= (pure f) <*> a <*> b <*> c <*> d <*> e

-- 4.1
infixl 5 <|*|>
fs <|*|> xs = [f x |x <- xs, f <- fs]

-- 5.1
data AT a = L a | B (AT a) (AT a)

instance Functor AT where
  fmap f (L a) = L (f a)
  fmap f (B t1 t2) = B (fmap f t1) (fmap f t2)

instance Applicative AT where
  pure = return
  (L f) <*> t = fmap f t
  t <*> (L x) = fmap ($ x) t
  B t1 t2 <*> t = B (t1 <*> t) (t2 <*> t)
  
instance Monad AT where
  return a = L a
  (L a) >>= f = f a
  (B t1 t2) >>= f = B (t1 >>= f) (t2 >>= f)

-- Foldable

-- 3.1
instance Foldable AT where
  foldMap f (L a) = f a
  foldMap f (B t1 t2) = foldMap f t1 <> foldMap f t2
  
treeFold :: (b -> b -> b) -> (a -> b) -> AT a -> b
treeFold fb fl = g where
  g (L a) = fl a
  g (B t1 t2) = fb (g t1) (g t2)

treeDepth :: AT a -> Int
treeDepth = treeFold (\x y -> 1 + max x y) (const 0)

-- Traversable

-- 1.1
instance Traversable AT where
  traverse f (L a) = L <$> f a
  traverse f (B t1 t2) = B <$> traverse f t1 <*> traverse f t2

-- 2.1
transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList
