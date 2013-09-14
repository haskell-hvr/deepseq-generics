{-# LANGUAGE BangPatterns, TypeOperators, FlexibleContexts #-}

-- |
-- Module:  Control.DeepSeq.Generics
-- Copyright:   (c) 2012, Herbert Valerio Riedel
-- License:     BSD-style (see the LICENSE file)
--
-- Maintainer:  Herbert Valerio Riedel <hvr@gnu.org>
-- Stability:   stable
-- Portability: GHC
--
-- Note: Beyond the primary scope of providing the 'genericRnf'
--       helper, this module also re-exports the definitions from
--       "Control.DeepSeq" for convenience. If this poses any
--       problems, just use qualified or explicit import statements
--       (see code usage example in the 'genericRnf' description)

module Control.DeepSeq.Generics
    ( genericRnf
      -- * "Control.DeepSeq" re-exports
    , deepseq
    , force
    , NFData(rnf)
    , ($!!)
    ) where

import Control.DeepSeq
import GHC.Generics

-- | "GHC.Generics"-based 'rnf' implementation
--
-- This provides a generic `rnf` implementation for one type at a
-- time. If the type of the value 'genericRnf' is asked to reduce to
-- NF contains values of other types, those types have to provide
-- 'NFData' instances. This also means that recursive types can only
-- be used with 'genericRnf' if a 'NFData' instance has been defined
-- as well (see examples below).
--
-- The typical usage for 'genericRnf' is for reducing boilerplate code
-- when defining 'NFData' instances for ordinary algebraic
-- datatypes. See the code below for some simple usage examples:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Control.DeepSeq
-- > import Control.DeepSeq.Generics (genericRnf)
-- > import GHC.Generics
-- >
-- > -- simple record
-- > data Foo = Foo AccountId Name Address
-- >          deriving Generic
-- >
-- > type Address      = [String]
-- > type Name         = String
-- > newtype AccountId = AccountId Int
-- >
-- > instance NFData AccountId
-- > instance NFData Foo where rnf = genericRnf
-- >
-- > -- recursive list-like type
-- > data N = Z | S N deriving Generic
-- >
-- > instance NFData N where rnf = genericRnf
-- >
-- > -- parametric & recursive type
-- > data Bar a = Bar0 | Bar1 a | Bar2 (Bar a)
-- >            deriving Generic
-- >
-- > instance NFData a => NFData (Bar a) where rnf = genericRnf
--
-- Note: The 'GNFData' type-class showing up in the type-signature is
--       used internally and not exported on purpose currently.

genericRnf :: (Generic a, GNFData (Rep a)) => a -> ()
genericRnf = grnf_ . from
{-# INLINE genericRnf #-}

-- | Hidden internal type-class
class GNFData f where
    grnf_ :: f a -> ()

instance GNFData V1 where
  grnf_ = undefined

instance GNFData U1 where
    grnf_ !U1 = ()
    {-# INLINE grnf_ #-}

instance NFData a => GNFData (K1 i a) where
    grnf_ = rnf . unK1
    {-# INLINE grnf_ #-}

instance GNFData a => GNFData (M1 i c a) where
    grnf_ = grnf_ . unM1
    {-# INLINE grnf_ #-}

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
    grnf_ (x :*: y) = grnf_ x `seq` grnf_ y
    {-# INLINE grnf_ #-}

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
    grnf_ (L1 x) = grnf_ x
    grnf_ (R1 x) = grnf_ x
    {-# INLINE grnf_ #-}
