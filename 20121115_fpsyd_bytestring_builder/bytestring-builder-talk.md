% Design and implementation of bytestring builders
% Simon Meier
% FP Sydney meetup - November 15th, 2012


My background
=============

-----------  ------------------------------------------------------------------------------------------------------------------------
2002 - 2007  Master of CS at ETH Zurich
2007 - 2013  PhD studies at ETH Zurich on automated security protocol verification (tool development using Haskell)
2013 - ....  full time Haskell programmer at [www.erudify.ch](http://www.erudify.ch) (we are looking for more Haskell programmers :-)
-----------  ------------------------------------------------------------------------------------------------------------------------

Currently traveling Australia for three months to celebrate the finished
thesis... yihaa! :-)

Bytestring builders
===================

- Datastructures for the efficient representation of sequences of bytes composed
  from smaller sequences of bytes.

- Typical use case: conversion of Haskell types to sequences of bytes,
  i.e., encodings

- Efficiency concerns:

    - *O(1)* append
    - efficient encoding of primitive Haskell types
    - chunk boundaries
    - redundant copying
    - buffer allocation


History of bytestring builders
==============================

-----------  --------------------------------------------------------------------------------
2000s        the `binary` library provides a bytestring builder
Aug 2010     blaze-builder developed from `binary` builder due to requirements in blaze-html GSoC
Nov 2010     factored out and polished `blaze-builder`
Nov 2011     rewrote blaze-builder for integration into the bytestring library
Jan 2012     finetuned internals of new bytestring builder
Sep 2012     Duncan Coutts released `bytestring-0.10.2.0` (contains *Nov 2011 patches* with some renamings)
-----------  --------------------------------------------------------------------------------

This talk is based on the Jan 2012 patches in the unreleased
[`bytestring-builder` package](https://github.com/meiersi/bytestring-builder/commit/7cba0e86bcef6959d72b90728009bfea7ef2dd7b).


The public API
==============

- encodings for primitive Haskell values, e.g., `charUtf8` and `wordHex`
- `Monoid` instance for composition
- execution by conversion to lazy bytestrings

~~~~ {.haskell}
import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Lazy.Builder       (charUtf8, toLazyByteString)
import           Data.ByteString.Lazy.Builder.ASCII (wordHex)
import           Data.Monoid                        ((<>))
import           Data.Foldable                      (foldMap)

escapeAndUtf8Encode :: String -> Builder
escapeAndUtf8Encode = foldMap escapeChar
  where
    escapeChar '\\' = charUtf8 '\\' <> charUtf8 '\\'
    escapeChar '\"' = charUtf8 '\\' <> charUtf8 '\"'
    escapeChar c    = charUtf8 c

test :: L.ByteString
test = toLazyByteString $ escapeAndUtf8Encode "λ-wörld " <> wordHex 4919

-- test == Chunk "\206\187-w\195\182rld 1337" Empty
~~~~

Internals: the idea
===================


  - builders are difference lists of buffer filling functions
      - ⇒ *O(1)* append

  - decompose execution
      - *driver*: manages buffer allocation
      - *build steps*: fill buffers and signal driver how to continue
      - ⇒ hoists state of allocation strategy out of inner loop
      - ⇒ builders become *independent* of buffer allocation strategy

  - support direct insertion of strict bytestrings
      - ⇒ avoid unnecessary copying


Internals: the types
====================

~~~~ {.haskell}
-- a range of a buffer (driver ensures liveness of ptrs)
data BufferRange =
    BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

-- a buffer filling action
type BuildStep a = BufferRange -> IO (BuildSignal a)

-- a signal to the driver providing the buffers
data BuildSignal a =
    Done        {-# UNPACK #-} !(Ptr Word8)   -- filled up to here
                                a             -- result (ignore for now)

  | BufferFull  {-# UNPACK #-} !Int           -- minimal size of next buffer
                {-# UNPACK #-} !(Ptr Word8)   -- filled up to here
                                (BuildStep a) -- continuation

  | InsertChunk {-# UNPACK #-} !(Ptr Word8)   -- filled up to here
                                S.ByteString  -- bytestring to insert
                                (BuildStep a) -- continuation

-- difference list of buffer filling actions for 0(1) append
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)
~~~~


Monoid instance
===============

~~~~ {.haskell}
instance Monoid Builder where
  mempty                            = Builder id
  mappend (Builder b1) (Builder b2) = Builder (\k -> b1 (b2 k))
~~~~

`mappend` is *fast but not free*

  - best case: completely optimized away
  - worst case: one closure allocation and two unknown function calls


Encoding primitive values
=========================

Many encodings of primitive values result in sequences of a *statically*
bounded size, e.g., `wordHex`

~~~~ {.haskell}
data BoundedEncoding a =
     BE {-# UNPACK #-} !Int                 -- bound
        (a -> Ptr Word8 -> IO (Ptr Word8))  -- encoding function
~~~~ {.haskell}

  - composed using combinators (bound remains static)

    ~~~~ {.haskell}
    contramapB :: (b -> a)          -> BoundedEncoding a -> BoundedEncoding b
    pairB      :: BoundedEncoding a -> BoundedEncoding b -> BoundedEncoding (a, b)
    ifB        :: (a -> Bool) -> BoundedEncoding a -> BoundedEncoding a -> BoundedEncoding a
    ~~~~

  - executed by conversion to builders

    ~~~~ {.haskell}
    encodeWithB           :: BoundedEncoding a     -> (a            -> Builder)
    -- variants exploiting spezialized optimizations
    encodeListWithB       :: BoundedEncoding a     -> ([a]          -> Builder)
    encodeByteStringWithB :: BoundedEncoding Word8 -> (S.ByteString -> Builder)
    ~~~~

  - great wrapper for custom C functions, e.g.,
    [V8's decimal encodings for IEEE floats](http://hackage.haskell.org/package/double-conversion)



Optimizations enabled by bounded-size encodings
===============================================

  - composing adjacent bounded encodings to save bounds checks
    (automated using rewriting rules)

  - tricky loop-hoisting

  - coupled indices for input and output arrays, used e.g. in

    ~~~~ {.haskell}
    encodeByteStringWithB :: BoundedEncoding Word8 -> S.ByteString -> Builder

    -- see https://github.com/meiersi/text/commit/b8c8f11923d46b5423ce13a98343865c209e53df
    -- 25% faster for UTF-8 encoding than existing encoder in Text library
    encodeUtf8Escaped :: BoundedEncoding Word8 -> Text -> Builder
    ~~~~

    **Idea:** use temporary end-of-output pointer `opeTmp` to ensure that the
    input-pointer `ip` is valid as long as `op < opeTmp`

    ~~~~
    input array:   |ip0   |ip        |ipe

    output buffer: |op0   |op        |opeTmp      |ope
    ~~~~

    **⇒ no `ip < ipe` check required in inner loop**


Example: fused UTF-8 encoding and HTML escaping of Text values
==============================================================

~~~ {.haskell}
import qualified Data.ByteString.Lazy.Builder.BasicEncoding  as E
-- see library docs for information on fixed-size encodings
import           Data.ByteString.Lazy.Builder.BasicEncoding
                 ( ifB, fromF, (>*<), (>$<) )

{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: E.BoundedEncoding Char
charUtf8HtmlEscaped =
    ifB (>  '>' ) E.charUtf8 $  -- '>' is the largest escaped 'Char'
    ifB (== '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    ifB (== '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    ifB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    ifB (== '"' ) (fixed5 ('&',('#',('3',('4',';'))))) $  -- &#34;
    (fromF E.char7)             -- fallback for remaining 'Char's
  where
    {-# INLINE fixed4 #-}
    fixed4 x = fromF $ const x >$<
      E.char7 >*< E.char7 >*< E.char7 >*< E.char7
    {-# INLINE fixed5 #-}
    fixed5 x = fromF $ const x >$<
      E.char7 >*< E.char7 >*< E.char7 >*< E.char7 >*< E.char7

textUtf8HtmlEscaped :: Text -> Builder
textUtf8HtmlEscaped = encodeUtf8Escaped charUtf8HtmlEscaped
~~~


Execution: considerations
=========================

  - controlling wasted space due to half-filled buffers

  - using pre-allocated buffers, e.g., the one of a `Handle`

  - interleaving builder execution and IO actions

~~~~ {.haskell}
data AllocationStrategy = AllocationStrategy
       (Maybe (Buffer, Int) -> IO Buffer) -- buffer allocator
       {-# UNPACK #-} !Int                -- default buffer size
       (Int -> Int -> Bool)               -- trimming predicate
~~~~

  - returning a result, e.g., the failure to encode λ using an ASCII code

    ~~~~ {.haskell}
    newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }
    ~~~~

    Note that `Put`s are not yet available in the public API





Execution: gory details
=======================

~~~~ {.haskell}
data ChunkIOStream a =
       Finished Buffer a                            -- last buffer and result
     | Yield1 S.ByteString (IO (ChunkIOStream a))   -- one chunk and continuation

buildStepToCIOS :: AllocationStrategy -> BuildStep a -> IO (ChunkIOStream a)
buildStepToCIOS (AllocationStrategy nextBuffer bufSize trim) =
    \step -> nextBuffer Nothing >>= fill step   -- allocate first buffer and fill
  where
    fill !step !buf@(Buffer fpbuf br@(BufferRange _ pe)) = do
        res <- fillWithBuildStep step doneH fullH insertChunkH br
        touchForeignPtr fpbuf  -- keep buffer alive
        return res
      where
        pbuf = unsafeForeignPtrToPtr fpbuf
        -- return a result
        doneH op' x = return $ Finished (Buffer fpbuf (BufferRange op' pe)) x
        -- handle a full buffer
        fullH op' minSize nextStep =
          | trim chunkSize size = do
              bs <- S.create chunkSize $ \pbuf' -> copyBytes pbuf' pbuf chunkSize
              return $ Yield1 bs k
          | otherwise           = return $ Yield1 (S.PS fpbuf 0 chunkSize) k
          where
            k = nextBuffer (Just (buf, max minSize bufSize)) >>= fill nextStep
            chunkSize = op' `minusPtr` pbuf
            size      = pe  `minusPtr` pbuf
        -- handle a direct chunk insertion
        insertChunkH op' bs nextStep = ....
~~~~


Comparison to the binary builder
================================

~~~~ {.haskell}
-- from binary-0.5.0.2
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

newtype Builder = Builder {
        runBuilder :: (Buffer -> IO L.ByteString)
                   -> (Buffer -> IO L.ByteString)
    }
~~~~

- `binary` builder code uses less recursion ⇒ easier to optimize for GHC
- fixed buffer allocation scheme
- more parameters to copy when calling the continuation
- no support for bounded-size encodings yet - easy to add




Conclusions
===========

  - use bytestring builders to construct bytestrings from small pieces
    - yields good performance by default
    - for very high performance: think about execution, optimize inner loops,
      avoid redundant copying/intermediate datastructures

  - long-term goal: every library that constructs bytestrings exposes conversions to bytestring builders
    - no unnecessary chunk boundaries
    - precise control over buffer allocation

In general: provide and use builder's to construct packed datastructures
