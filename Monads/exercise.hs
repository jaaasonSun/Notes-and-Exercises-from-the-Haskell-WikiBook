import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Functor.Identity
import Control.Monad.Trans.Class

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n $ randomRIO (1, 6)

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice gen = ((n1, n2), gen'') where
  (n1, gen') = randomR (1, 6) gen
  (n2, gen'') = randomR (1, 6) gen'
                    

rollNDice :: Int -> State StdGen [Int]
rollNDice n = replicateM n rollDie where
  rollDie = state $ randomR (1, 6)

digit :: Int -> String -> Maybe Int
digit _ []                     = Nothing
digit i (c:_) | i > 9 || i < 0 = Nothing
              | otherwise      =
  if [c] == show i then Just i else Nothing

char :: Char -> String -> Maybe (Char, String)
char c s = do
  c' : s' <- return s
  guard (c == c')
  return (c, s')

hexChar :: String -> Maybe (Char, String)
hexChar s = parseDigit s `mplus` parseAlpha s where
  parseDigit s = do
    let (c:s') = s
    d <- msum $ map ($ s) $ map digit [0..9] 
    return (intToDigit d, s')
  parseAlpha s = msum $ map ($ s) $ map char (['a'..'f'] ++ ['A'..'F'])


newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Monad m => Functor (IdentityT m) where
  fmap = liftM

instance Monad m => Applicative (IdentityT m) where
  pure = return
  (<*>) = Control.Monad.ap

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return
  x >>= f = IdentityT $ runIdentityT x >>= (runIdentityT . f)

instance MonadTrans IdentityT where
  lift m = IdentityT m
