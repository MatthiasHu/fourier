
import Data.Complex
import Data.Ratio
import Control.Applicative
import Data.Foldable (fold)
import Data.Maybe (fromJust)


class (Functor s) => SignalLike s where
  start  :: s a -> Maybe Int
  stop   :: s a -> Maybe Int
  sample :: s a -> Int -> a

validIndex :: (SignalLike s) => s a -> Int -> Bool
validIndex s i = not tooSmall && not tooBig
  where
    tooSmall = maybe False (>i) (start s)
    tooBig   = maybe False (<i) (stop  s)

sample' :: (SignalLike s, Num a) => s a -> Int -> a
sample' s i = if validIndex s i then sample s i else 0

instance SignalLike [] where
  start _ = Just 0
  stop  l = Just (length l - 1)
  sample = (!!)

signalToList :: (SignalLike s) => s a -> Maybe [a]
signalToList s = do
  a <- start s
  b <- stop s
  return [ sample s i | i <- [a .. b] ]

signalToList' :: (SignalLike s) => s a -> [a]
signalToList' = fromJust . signalToList

instance (Num n) => SignalLike ((->) n) where
  start _ = Nothing
  stop  _ = Nothing
  sample  = (. fromIntegral)

data Signal a = Signal
  { signalStart  :: Maybe Int
  , signalStop   :: Maybe Int
  , signalSample :: Int -> a
  }

instance Functor Signal where
  fmap f (Signal a b x) = Signal a b (f . x)

instance SignalLike Signal where
  start  = signalStart
  stop   = signalStop
  sample = signalSample

instance (Show a) => Show (Signal a) where
  show s = fold [show a, "--", show b, ": ", show list]
    where
      Just a = start s
      Just b = stop s
      Just list = signalToList s

add :: (SignalLike s, SignalLike t, Num a) => s a -> t a -> Signal a
add s t = Signal
  { signalStart  = liftA2 min (start s) (start t)
  , signalStop   = liftA2 max (stop  s) (stop  t)
  , signalSample = \i -> sample' s i + sample' t i
  }

mult :: (SignalLike s, SignalLike t, Num a) => s a -> t a -> Signal a
mult s t = Signal
  { signalStart  = ifBothJust max (start s) (start t)
  , signalStop   = ifBothJust min (stop  s) (stop  t)
  , signalSample = \i -> sample' s i * sample' t i
  }
  where
    ifBothJust f (Just a) (Just b) = Just (f a b)
    ifBothJust _ (Just a) Nothing  = Just a
    ifBothJust _ Nothing  (Just b) = Just b
    ifBothJust _ Nothing  Nothing  = Nothing

shift :: (SignalLike s) => Int -> s a -> Signal a
shift d s = Signal
  { signalStart  = (+d) <$> start s
  , signalStop   = (+d) <$> stop s
  , signalSample = sample s . (+ negate d)
  }

flipSignal :: (SignalLike s) => s a -> Signal a
flipSignal s = Signal
  { signalStart  = negate <$> stop s
  , signalStop   = negate <$> start s
  , signalSample = sample s . negate
  }

summate :: (SignalLike s, Num a) => s a -> a
summate = sum . signalToList'

multSum :: (SignalLike s, SignalLike t, Num a) => s a -> t a -> a
multSum s t = summate $ mult s t

convolve :: (SignalLike s, SignalLike t, Num a) =>
  s a -> t a -> Signal a
convolve s t = Signal
  { signalStart  = liftA2 (+) (start s) (start t)
  , signalStop   = liftA2 (+) (stop  s) (stop  t)
  , signalSample = \d -> multSum s (shift d $ flipSignal t)
  }

circleSignal :: (RealFloat a) => a -> a -> Complex a
circleSignal f t = cis (2*pi*f*t)

-- Discrete Fourier Transform (naive implementation)
dft :: (SignalLike s, RealFloat a) =>
  s (Complex a) -> Signal (Complex a)
dft s = Signal
  { signalStart  = Just 0
  , signalStop   = Just (n-1)
  , signalSample = \f -> multSum s (circle f)
  }
  where
    n = fromJust (stop s) - fromJust (start s) + 1
    circle f = circleSignal (fromIntegral f / fromIntegral n)
