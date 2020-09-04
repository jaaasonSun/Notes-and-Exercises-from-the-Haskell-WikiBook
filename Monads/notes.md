# Notes from chapter 4: Monads

## Applicative Functor

``` haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
pure :: Applicative f => a -> f a
<$> :: Functor f => (a -> b) -> f a -> f b
(*>) :: Applicative f => f a -> f b -> f b
```

## Monad

``` haskell
class Applicative m => Monad m where
    return :: Monad m => a -> ma
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    
    (>>) :: m a -> m b -> m b
    fail :: string -> m a
```

### Monad Laws

- `return` is left and right unit of `>>=`
- `>>=` is associative

``` haskell
m >>= return  = m
return x >>= f = fx
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

### Additional Functions

``` haskell
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c

liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
ap :: Monad m => m (a -> b) -> ma -> mb
```

## Alternative and MonadPlus

``` haskell
class Applicative f => Alternative f where
    empty :: f a 
    (<|> :: f a -> f a -> f a)
```

``` haskell
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a 
```

### Laws

- `<|>` is associative
- `empty` is left and right identity of `<|>`

- `mplus` is associative
- `mzero` is left and right identity of `mplus`
- `mzero` is left and right zero of `(>>=)`

### Additional Functions

``` haskell
asum :: (Alternative f, Foldable t) => t (f a) -> f a
asum = foldr (<|>) empty

msum :: (Monad f, Foldable t) => t (m a) -> m a

guard :: Alternative m => Bool -> m ()
guard True = pure()
guard _ = empty
```
