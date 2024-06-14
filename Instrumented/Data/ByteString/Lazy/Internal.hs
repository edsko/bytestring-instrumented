{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Unsafe #-}

#ifdef HS_BYTESTRING_ASSERTIONS
{-# LANGUAGE PatternSynonyms #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Instrumented.Data.ByteString.Lazy.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Instrumented.Data.ByteString.Lazy.Internal (

        -- * The lazy @ByteString@ type and representation
        ByteString(Empty, Chunk),
        LazyByteString,
        chunk,
        foldrChunks,
        foldlChunks,

        -- * Data type invariant and abstraction function
        invariant,
        checkInvariant,

        -- * Chunk allocation sizes
        defaultChunkSize,
        smallChunkSize,
        chunkOverhead,

        -- * Conversion with lists: packing and unpacking
        packBytes, packChars,
        unpackBytes, unpackChars,
        -- * Conversions with strict ByteString
        fromStrict, toStrict,

  ) where

import Prelude hiding (concat)

import qualified Instrumented.Data.ByteString.Internal.Type as S

import Data.Word (Word8)
import Foreign.Storable (Storable(sizeOf))

#if MIN_VERSION_base(4,13,0)
#else
import Data.Semigroup   (Semigroup ((<>), sconcat, stimes))
#endif





#ifdef HS_BYTESTRING_ASSERTIONS
import Control.Exception (assert)
#endif

import Data.ByteString.Lazy.Internal (ByteString(..))

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.
--
-- A 'LazyByteString' contains 8-bit bytes, or by using the operations
-- from "Instrumented.Data.ByteString.Lazy.Char8" it can be interpreted as containing
-- 8-bit characters.
--



-- | Type synonym for the lazy flavour of 'ByteString'.
--
-- @since 0.11.2.0
type LazyByteString = ByteString

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenBytes n cs of
      (bs, [])  -> chunk bs Empty
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) smallChunkSize) cs')

packChars :: [Char] -> ByteString
packChars cs0 = packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenChars n cs of
      (bs, [])  -> chunk bs Empty
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) smallChunkSize) cs')

unpackBytes :: ByteString -> [Word8]
unpackBytes Empty        = []
unpackBytes (Chunk c cs) = S.unpackAppendBytesLazy c (unpackBytes cs)

unpackChars :: ByteString -> [Char]
unpackChars Empty        = []
unpackChars (Chunk c cs) = S.unpackAppendCharsLazy c (unpackChars cs)

------------------------------------------------------------------------

-- We no longer use these invariant-checking functions internally,
-- preferring an assertion on `Chunk` itself, controlled by the
-- HS_BYTESTRING_ASSERTIONS preprocessor macro.

-- | The data type invariant:
-- Every ByteString is either 'Empty' or consists of non-null
-- 'S.StrictByteString's. All functions must preserve this.
--
invariant :: ByteString -> Bool
invariant Empty                     = True
invariant (Chunk (S.BS _ len) cs) = len > 0 && invariant cs

-- | Lazily checks that the given 'ByteString' satisfies the data type's
-- "no empty chunks" invariant, raising an exception in place of the
-- first chunk that does not satisfy the invariant.
checkInvariant :: ByteString -> ByteString
checkInvariant Empty = Empty
checkInvariant (Chunk c@(S.BS _ len) cs)
    | len > 0   = Chunk c (checkInvariant cs)
    | otherwise = error $ "Instrumented.Data.ByteString.Lazy: invariant violation:"
               ++ show (Chunk c cs)

------------------------------------------------------------------------

-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: S.StrictByteString -> ByteString -> ByteString
chunk c@(S.BS _ len) cs | len == 0  = cs
                        | otherwise = Chunk c cs
{-# INLINE chunk #-}

-- | Consume the chunks of a lazy ByteString with a natural right fold.
foldrChunks :: (S.StrictByteString -> a -> a) -> a -> ByteString -> a
foldrChunks f z = go
  where go Empty        = z
        go (Chunk c cs) = f c (go cs)
{-# INLINE foldrChunks #-}

-- | Consume the chunks of a lazy ByteString with a strict, tail-recursive,
-- accumulating left fold.
foldlChunks :: (a -> S.StrictByteString -> a) -> a -> ByteString -> a
foldlChunks f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (f a c) cs
{-# INLINE foldlChunks #-}

------------------------------------------------------------------------

-- The representation uses lists of packed chunks. When we have to convert from
-- a lazy list to the chunked representation, then by default we use this
-- chunk size. Some functions give you more control over the chunk size.
--
-- Measurements here:
--  http://www.cse.unsw.edu.au/~dons/tmp/chunksize_v_cache.png
--
-- indicate that a value around 0.5 to 1 x your L2 cache is best.
-- The following value assumes people have something greater than 128k,
-- and need to share the cache with other programs.

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead 
   where k = 1024

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------
-- Implementations for Eq, Ord and Monoid instances


------------------------------------------------------------------------
-- Conversions

-- |/O(1)/ Convert a 'S.StrictByteString' into a 'LazyByteString'.
fromStrict :: S.StrictByteString -> LazyByteString
fromStrict (S.BS _ 0) = Empty
fromStrict bs = Chunk bs Empty

-- |/O(n)/ Convert a 'LazyByteString' into a 'S.StrictByteString'.
--
-- Note that this is an /expensive/ operation that forces the whole
-- 'LazyByteString' into memory and then copies all the data. If possible, try to
-- avoid converting back and forth between strict and lazy bytestrings.
--
toStrict :: LazyByteString -> S.StrictByteString
toStrict = \cs -> goLen0 cs cs
    -- We pass the original [ByteString] (bss0) through as an argument through
    -- goLen0, goLen1, and goLen since we will need it again in goCopy. Passing
    -- it as an explicit argument avoids capturing it in these functions'
    -- closures which would result in unnecessary closure allocation.
  where
    -- It's still possible that the result is empty
    goLen0 _   Empty                 = S.BS S.nullForeignPtr 0
    goLen0 cs0 (Chunk c cs)          = goLen1 cs0 c cs

    -- It's still possible that the result is a single chunk
    goLen1 _   bs Empty = bs
    goLen1 cs0 (S.BS _ bl) (Chunk (S.BS _ cl) cs) =
        goLen cs0 (S.checkedAdd "Lazy.toStrict" bl cl) cs

    -- General case, just find the total length we'll need
    goLen cs0 !total (Chunk (S.BS _ cl) cs) =
      goLen cs0 (S.checkedAdd "Lazy.toStrict" total cl) cs
    goLen cs0 total Empty =
      S.unsafeCreateFp total $ \ptr -> goCopy cs0 ptr

    -- Copy the data
    goCopy Empty                    !_   = return ()
    goCopy (Chunk (S.BS _  0  ) cs) !ptr = goCopy cs ptr
    goCopy (Chunk (S.BS fp len) cs) !ptr = do
      S.memcpyFp ptr fp len
      goCopy cs (ptr `S.plusForeignPtr` len)
-- See the comment on Instrumented.Data.ByteString.Internal.concat for some background on
-- this implementation.
