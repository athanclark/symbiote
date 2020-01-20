{-|

Module: Test.Serialization.Symbiote.Debug
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

Data types that are used by the underlying protocol facilitating communication. See
'Test.Serialization.Symbiote.WebSocket' and 'Test.Serialization.Symbiote.ZeroMQ' for
details.

-}

module Test.Serialization.Symbiote.Debug where

-- | Data type for deciding how verbose logging should be.
data Debug = FullDebug | Percent | NoDebug

-- | Data type determining if this is used on a public network or private one
data Network = Public | Private
  deriving (Eq)
