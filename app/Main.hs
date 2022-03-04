{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (length)
import Control.Monad (unless, void, foldM_, foldM)

import GHC.Exts
import GHC.IO (IO(..))
import Data.List (unfoldr)
import Control.DeepSeq
import System.Random
import System.Random.Stateful
import Data.Foldable
import qualified Data.ByteString as B
import Data.Word
import Crypto.Hash

data Array             a = A   (Array#      a)
data SmallArray        a = SA  (SmallArray# a)
data MutableArray      a = MA  (MutableArray#      RealWorld a)
data SmallMutableArray a = SMA (SmallMutableArray# RealWorld a)


main :: IO ()
main = do
    gen <- getStdGen
    ---------------------- Mutable Constructions --------------------------------
    let low  = 0
        n    = 5000
        size = case n of (I# i) -> i
        element :: SHA512
        element = hash . B.pack . pure $ (0 :: Word8)
        es :: [SHA512]
        es   = map hash $ randomBStrings n gen

    smArr@(SMA smarr# ) <- IO (\s0 -> case newSmallArray# size element s0 of
                                  (# s1, ba# #) -> (# s1, SMA ba# #))

    mArr@(MA mar#) <- IO (\s0 -> case newArray# size element s0 of
                             (# s1, ba# #) -> (# s1, MA ba# #))

    ----------------------------- Writes ---------------------------------------
    foldM_ (\unit (ix, ele) -> writeSMA smArr ix ele) () $ zip [0..n-1] es


    arr  <- IO (\s0 -> case unsafeFreezeArray# mar# s0 of
                        (# s1, ma# #) -> (# s1, A ma# #))

    sArr <- IO (\s0 -> case unsafeFreezeSmallArray# smarr# s0 of
                        (# s1, sma# #) -> (# s1, SA sma# #))

    ----------------------------- Sum ---------------------------------------
    -- we do a selection sort, the idea here is that by sorting we perterbate
    -- the array enough so that elements now point to "random" regions of the
    -- heap. Or, a better way to think about it, is that now the array is
    -- shuffled such that each element *is not* read in allocation order.
    let perterb = shuffleArr : shuffleArr : perterb
    -- printSMA smArr
    -- putStrLn "\n----------------------\n"

    traverse_ ($! smArr) (take 120 perterb)

    -- putStrLn "\n----------------------\n"
    -- printSMA smArr

    -- shuffleArr smArr

    -- putStrLn "\n----------------------\n"
    -- printSMA smArr

    -- sum the array
    -- randomSum smArr n >>= print
    -- randomSumP smArr n >>= print

    -- let sumM (a,b) y = return $! b + y + a

    -- foldlSMA'  sumM 0 smArr >>= print
    -- foldlSMA'P sumM 0 smArr >>= print

    ---------------------------- Hashing ---------------------------------------
    -- showSMA' smArr
    showSMA'P smArr



------------------------------ Array helpers -----------------------------------
writeSMA :: SmallMutableArray a -> Int -> a -> IO ()
writeSMA (SMA ba#) (I# i#) e = IO (\s ->
    case writeSmallArray# ba# i# e s of
      s' -> (# s', () #))


writeMA :: MutableArray a -> Int -> a -> IO ()
writeMA (MA ba#) (I# i#) e = IO (\s ->
    case writeArray# ba# i# e s of
      s' -> (# s', () #))


readSMA :: SmallMutableArray a -> Int -> IO a
readSMA (SMA ba#) (I# i#) = IO (\s -> readSmallArray# ba# i# s)

readMA :: MutableArray a -> Int -> IO a
readMA (MA ba#) (I# i#) = IO (\s -> readArray# ba# i# s)

readSA :: SmallArray a -> Int -> IO a
readSA (SA ba#) (I# i#) = IO (\s -> case indexSmallArray# ba# i# of
                                 (# i #) -> (# s, i #))

readA :: Array a -> Int -> IO a
readA (A ba#) (I# i#) = IO (\s -> case indexArray# ba# i# of
                               (# i #) -> (# s, i #))

printSMA :: Show a => SmallMutableArray a -> IO ()
printSMA = foldlSMA' go ()
  where go ele _ = print ele


prefetchSmallMutableArrayBy
  :: (SmallMutableArray# RealWorld a -> Int# -> State# RealWorld -> State# RealWorld)
  -> Int -- offset
  -> SmallMutableArray a
  -> IO (SmallMutableArray a)
prefetchSmallMutableArrayBy prefetch (I# offset) (SMA sma#) = IO (\s -> (# prefetch sma# offset s, SMA sma# #))

prefetchSmallMutableArray :: Int -> SmallMutableArray a -> IO ()
{-# INLINE prefetchSmallMutableArray #-}
prefetchSmallMutableArray = (void .) . prefetchSmallMutableArrayBy prefetchSmallMutableArray3#

prefetchSmallMutableArray' :: Int -> SmallMutableArray a -> SmallMutableArray a
prefetchSmallMutableArray' = (inlinePerformIO .) . prefetchSmallMutableArrayBy prefetchSmallMutableArray3#



prefetchMutableArrayBy
  :: (MutableArray# RealWorld a -> Int# -> State# RealWorld -> State# RealWorld)
  -> MutableArray a
  -> IO (MutableArray a)
prefetchMutableArrayBy prefetch (MA ma#)= IO (\s -> (# prefetch ma# 0# s, MA ma# #))

prefetchMutableArray :: MutableArray a -> IO ()
prefetchMutableArray = void . prefetchMutableArrayBy prefetchMutableArray1#

prefetchMutableArray' :: MutableArray a -> MutableArray a
prefetchMutableArray' = inlinePerformIO . prefetchMutableArrayBy prefetchMutableArray1#

prefetchValueBy :: (a -> State# RealWorld -> State# RealWorld) -> a -> IO ()
prefetchValueBy prefetch a = IO (\s -> (# prefetch a s, () #))

prefetchValue3 :: a -> IO ()
prefetchValue3 = prefetchValueBy prefetchValue3#

prefetchValue0 :: a -> IO ()
prefetchValue0 = prefetchValueBy prefetchValue0#

prefetchSmallArrayBy
  :: (SmallArray# a -> Int# -> State# RealWorld -> State# RealWorld)
  -> SmallArray a
  -> IO (SmallArray a)
prefetchSmallArrayBy prefetch (SA sa#)= IO (\s -> (# prefetch sa# 0# s, SA sa# #))

prefetchSmallArray :: SmallArray a -> IO ()
prefetchSmallArray = void . prefetchSmallArrayBy prefetchSmallArray2#

prefetchSmallArray' :: SmallArray a -> SmallArray a
prefetchSmallArray' = inlinePerformIO . prefetchSmallArrayBy prefetchSmallArray2#

prefetchArrayBy
  :: (Array# a -> Int# -> State# RealWorld -> State# RealWorld)
  -> Array a
  -> IO (Array a)
prefetchArrayBy prefetch (A sa#)= IO (\s -> (# prefetch sa# 100# s, A sa# #))

prefetchArray :: Array a -> IO ()
prefetchArray = void . prefetchArrayBy prefetchArray3#

prefetchArray' :: Array a -> Array a
prefetchArray' = inlinePerformIO . prefetchArrayBy prefetchArray3#

length :: SmallArray# Int -> Int
length ary = I# (sizeofSmallArray# ary)
{-# INLINE length #-}

lengthSMA :: SmallMutableArray a -> Int
lengthSMA (SMA ary) = I# (sizeofSmallMutableArray# ary)
{-# INLINE lengthSMA #-}

index# :: SmallArray# (Int,Int) -> Int -> (# (Int,Int) #)
index# ary _i@(I# i#) = indexSmallArray# ary i#
{-# INLINE index# #-}

swap :: Int -> Int -> SmallMutableArray a -> IO (SmallMutableArray a)
swap (I# ixFrom) (I# ixTo) (SMA ar#) = IO $ \s ->
  case readSmallArray# ar# ixFrom s of
    (# s', eleFrom #) ->
      case readSmallArray# ar# ixTo s' of
        (# s'', eleTo #) ->
          case writeSmallArray# ar# ixFrom eleTo s'' of
            s''' ->
              (# writeSmallArray# ar# ixTo eleFrom s''', SMA ar# #)

sortArr :: Ord a => SmallMutableArray a -> IO (SmallMutableArray a)
sortArr x@(SMA ar#) = do
  let limit = sizeofSmallMutableArray# ar#

      argMin :: Int -> IO Int
      argMin ix@(I# index) = do let indices = [ix..I# limit-1] -- all indices from current index

                                    -- find and store minimum element and index by comparing with current accumulator
                                    -- fst is element, snd is old index
                                    go old@(acc,_) ix_@(I# ix') = IO $ \s ->
                                      case readSmallArray# ar# ix' s of
                                        (# s' , newEle #) -> (# s'
                                                             , if acc <= newEle then old else (newEle, ix_)
                                                             #)

                                -- get initial values
                                initAcc <- IO $ \st ->
                                  case readSmallArray# ar# index st of
                                    (# s', init_ele #) -> (# s', (init_ele, ix) #)


                                -- kick off, snd to discard the element and keep the index
                                snd <$> foldM go initAcc indices

      sort :: Int -> IO ()
      sort ix =
        if ix == I# limit
        then return ()
        else do minIx <- argMin ix
                swap ix minIx x
                sort (succ ix)

  sort 0
  return x

shuffleArr :: SmallMutableArray a -> IO (SmallMutableArray a)
shuffleArr sma@(SMA ar#) = do
  gen <- getStdGen
  let limit = I# (sizeofSmallMutableArray# ar#) - 1
      half  = limit `div` 2
      ixs'  = take limit $ unfoldr (Just . uniformR (0, limit)) gen
      ixs   = zip (take half ixs') (drop half ixs')
      go arr (from,to) = swap to from arr
  foldM_ go sma ixs
  return sma
------------------------------ Array helpers -----------------------------------


---------------------------- Some Computations ---------------------------------
-- a fold over small mutable arrays with no prefetching.
foldlSMA' :: (a -> b -> IO b) -> b -> SmallMutableArray a -> IO b
foldlSMA' f acc ary0 = go (lengthSMA ary0 - 1) acc
  where
    go 0  !z = return z
    go i   z = readSMA ary0 i >>= flip f z >>= go (min (i-1) 0)
{-# INLINE foldlSMA' #-}

-- a fold over small mutable arrays with prefetching.
foldlSMA'P :: (a -> b -> IO b) -> b -> SmallMutableArray a -> IO b
foldlSMA'P f acc ary0 = go l acc
  where
    l = lengthSMA ary0 - 1
    go 0 !z = return z
    go i  z = do prefetchSmallMutableArray (i-3) ary0
                 readSMA ary0 i >>= flip f z >>= go (min (i-1) 0)
{-# INLINE foldlSMA'P #-}

showSMA' :: Show a => SmallMutableArray a -> IO ()
showSMA' ary0 = go (lengthSMA ary0 - 1)
  where
    go 0  = return ()
    go i  = readSMA ary0 i >>= print >> go (i-1)
{-# INLINE showSMA' #-}

showSMA'P :: Show a => SmallMutableArray a -> IO ()
showSMA'P ary0 = go (lengthSMA ary0 - 1)
  where
    go 0  = return ()
    go i  = prefetchSmallMutableArray (i-3) ary0 >> readSMA ary0 i >>= print >> go (i-1)
{-# INLINE showSMA'P #-}

randomSum' :: SmallMutableArray (Int,Int) -> Int -> IO Int
randomSum' arr times = do
  gen <- getStdGen
  let genIx = randomR (0, lengthSMA arr - 1)
      go !acc 0   _           = return acc
      go  acc ctr (ix, ixGen) = do let next = genIx ixGen
                                   (x,y) <- readSMA arr ix
                                   go (x + acc + y) (ctr-1) next
  go 0 times (genIx gen)


randomSum'P :: SmallMutableArray (Int,Int) -> Int -> IO Int
randomSum'P arr times = do
  gen <- getStdGen
  let genIx = randomR (0, lengthSMA arr - 1)
      go !acc 0   _           = return acc
      go  acc ctr (ix, ixGen) = do let next@(ix',_) = genIx ixGen
                                   prefetchSmallMutableArray ix' arr
                                   (x,y) <- readSMA arr ix
                                   go (x + acc + y) (ctr-1) next
  go 0 times (genIx gen)



randomSum :: SmallMutableArray (Int,Int) -> Int -> IO Int
randomSum arr times = do
  gen <- getStdGen
  let ixs = force $ take times $ unfoldr (Just . uniformR (0, lengthSMA arr - 1)) gen
      go acc [] = return acc
      go acc (ix:ixs) = do (x,y) <- readSMA arr ix
                           go (x + acc + y) ixs
  go 0 ixs

randomSumP :: SmallMutableArray (Int,Int) -> Int -> IO Int
randomSumP arr times = do
  gen <- getStdGen
  let ixs = force $ take times $ unfoldr (Just . uniformR (0, lengthSMA arr - 1)) gen
      go acc [] = return acc
      go acc (ix:rest@(_:_:ix0:_:_:ix1:ixs)) =
        do (x,y) <- readSMA arr ix
           prefetchSmallMutableArray ix0 arr
           prefetchSmallMutableArray ix1 arr
           go (x + acc + y) rest
      go acc (ix:ixs) = do (x,y) <- readSMA arr ix
                           go (x + acc + y) ixs
  go 0 ixs
---------------------------- Some Computations ---------------------------------


---------------------------- Utils ---------------------------------------------
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

-- | an infinite stream of bytes and their generators
randomBytes :: StdGen -> [(StdGen, Word8)]
randomBytes g = (g, fromIntegral value) : randomBytes next
  where (value, next) = genWord8 g

-- | get cnt number of random byte strings
randomBStrings :: Int -> StdGen -> [B.ByteString]
randomBStrings 0   g = mempty
randomBStrings cnt g = B.pack bs : randomBStrings (cnt-1) next
  where (bs',(next,_):_) = splitAt 2048 $ randomBytes g
        bs               = fmap snd bs'
---------------------------- Utils ---------------------------------------------
