module Fourier where

import Data.Complex
import Data.Array
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

signalLength :: (SignalLike s) => s a -> Int
signalLength s = fromJust (stop s) - fromJust (start s) + 1

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
  { signalStart  = start s
  , signalStop   = stop s
  , signalSample = \f -> multSum s (circle f)
  }
  where
    n = signalLength s
    circle f = circleSignal (fromIntegral f / fromIntegral n)

symmetrice :: (SignalLike s) => s a -> Signal a
symmetrice s = Signal
  { signalStart  = Just $ -(n-2)
  , signalStop   = Just $ n-1
  , signalSample = \i -> sample s $ a + (abs i `mod` n)
  }
  where
    n = signalLength s
    a = fromJust (start s)

-- Discrete Cosine Transform (variant DCT-I),
-- inefficiently implemented using dft
dct :: (SignalLike s, RealFloat a) => s a -> Signal a
dct s = fmap ((/2) . realPart) $
  (dft $ symmetrice (fmap (:+ 0) s)) {signalStart = Just 0}

dct2d :: (RealFloat a) =>
  (Int, Int) -> ((Int, Int) -> a) -> (Int, Int) -> a
dct2d (w, h) f = f''
  where
    f'0 (x, y) =
      sample (dct $ Signal (Just 0) (Just (w-1)) (\i -> f (i, y))) x
    arr = array ((0, 0), (w-1, h-1))
      [ ((x, y), f'0 (x, y)) | x <- [0..w-1], y <- [0..h-1] ]
    f' = (arr !)
    f'' (x, y) =
      sample (dct $ Signal (Just 0) (Just (h-1)) (\i -> f' (x, i))) y

-- remove the scaling from applying dct twice
unscaleDct :: (SignalLike s, Fractional a) => s a -> s a
unscaleDct s = fmap (*scale) s
  where
    scale = 2 / fromIntegral (signalLength s - 1)
