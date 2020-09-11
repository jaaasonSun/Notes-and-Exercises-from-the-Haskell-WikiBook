# Advanced Haskell

## Monoid

``` haskell
class Monoid a where
    mempty :: a 
    mappend :: a -> a -> a -- (<>)
    
    mconcat :: [a] -> a 
    mconcat = foldr mappend mempty
```

### Monoid Laws

- associativity
- `mempty` is left and right identity of `(<>)`

``` haskell
(x <> y) <> z = x <> (y <> z)
mempty <> x = x
x <> mempty = x
```

## Applicative Functor

``` haskell
class (Functor f) => Applicative f where
    pure :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b
```

### Applicative Laws 

``` haskell
pure id <*> v = v 
    -- Identity
pure f <*> pure x = pure (f x)
    -- Homonorphism
u <*> pure y = pure ($ y) <*> u 
    -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Composition
```

### Commutatiity

``` haskell
f <$> u <*> v = flip f <$> v <*> u
```

Convention in Haskell is to implement `<*>`, `*>`, `<*` and other operators using left-to-right sequencing. `<**>` inverts sequencing. This convention follows from the fact that `<*>` is implemented with `>>=` which is natually left-to-right, so in order for Applicative Functors to be coherent with Monads, they implement left-toright sequencing.

## Monoidal

``` haskell
class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a, b)
```

### Monoidal Laws

``` haskell
fmap snd $ unit *&* v = v
    -- left identity
fmap fst $ v *&* unit = v
    -- right identity
fmap asl $ u *&* (v *&* w) = (u *&* v) *&* w
    -- associativity
    
-- additionally
fmap (g *** h) (u *&* v) = fmap g u *&* fmap h v

-- where
asl (x, (y z)) = ((x, y), z)
g *** h = \(x, y) -> (g x, h y)
```

 ## Foldable

``` haskell
foldMap :: Moniod m => (a -> m) -> t a -> m
foldMap g = mconcat . fmap g

newtype Endo b = Endo { appEndo :: b -> b }

instance Monoid Endo where
    mempty = Endo id
    Endo g `mappend` Endo f = Endo (g . f)
    
foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
foldComposing f = foldMap (Endo . f)

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z xs = appEndo (foldComposing f xs) z
```

## Traversable

``` haskell
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)
```

### Traversable Laws

``` haskell
traverse Identity = Identity
    -- identity
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    -- composition

-- if t is an aplicative homomorphism
t . traverse f = traverse (t . f)
    -- naturality
```

## Arrow

``` haskell
class Category y where
    id :: y a a 
    (.) :: y b c -> y a b -> y a c
    
class Category y => Arrow y where
    arr :: (a -> b) -> y a b 
    first :: y a b -> y (a, c) (b, c)
    
    second :: y a b -> y (c, a) (c, b)
    (***) :: y a c -> y b d -> y (a, b) (c, d)
    (&&&) :: y a b -> y a c -> y a (b, c)
    
class Arrow y => ArrowChoice y where
    left :: y a b -> y (Either a c) (Either b c)
    
    right :: y a b -> y (Either c a) (Either c b)
    (+++) :: y a c -> y b d -> y (Either a b) (Either c d)
    (|||) :: y a c -> y b c -> y (Either a b) c

class Arrow y => ArrowApply y where
    app :: y (y a b, a) b
```

## Cont Monad

``` haskell
cont :: ((a -> r) -> r) -> Cont r a 
runCont :: Cont r a -> (a -> r) -> r 

instance Monad (Cont r) where 
    return x = cont ($ x)
    s >>= f = cont $ \c -> runCont s $ \x -> runCont (f x) c
    
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
```
