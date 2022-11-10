{-# LANGUAGE StrictData #-}

module IRI.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16)
import Net.IPv4 (IPv4)
import Net.IPv6 (IPv6)

data IRI = IRI
  { scheme :: Scheme
  , hierarchy :: Hierarchy
  , query :: Query
  , fragment :: Fragment
  }
  deriving stock (Eq, Ord, Show)

newtype Scheme = Scheme ByteString
  deriving newtype (Eq, Ord, Show)

data Hierarchy
  = AuthorisedHierarchy Authority Path
  | AbsoluteHierarchy Path
  | RelativeHierarchy Path
  deriving stock (Eq, Ord, Show)

data Authority
  = Authority UserInfo Host Port
  deriving stock (Eq, Ord, Show)

data UserInfo
  = PresentUserInfo User Password
  | MissingUserInfo
  deriving stock (Eq, Ord, Show)

newtype User
  = User ByteString
  deriving newtype (Eq, Ord, Show)

data Password
  = PresentPassword ByteString
  | MissingPassword
  deriving stock (Eq, Ord, Show)

data Host
  = NamedHost RegName
  | IpV4Host IPv4
  | IpV6Host IPv6
  deriving stock (Eq, Ord, Show)

newtype RegName = RegName (Vector DomainLabel)
  deriving newtype (Eq, Ord, Show)

newtype DomainLabel = DomainLabel Text
  deriving newtype (Eq, Ord, Show)

data Port
  = PresentPort Word16
  | MissingPort
  deriving stock (Eq, Ord, Show)

newtype Path
  = Path (Vector PathSegment)
  deriving newtype (Eq, Ord, Show)

newtype PathSegment
  = PathSegment ByteString
  deriving newtype (Eq, Ord, Show)

{-|
Since the exact structure of the query string is not standardised and
methods used to parse the query string may differ between websites,
we simply represent it as percent-decoded bytes.

See <https://en.wikipedia.org/wiki/Query_string>.
-}
newtype Query
  = Query ByteString
  deriving newtype (Eq, Ord, Show)

newtype Fragment
  = Fragment ByteString
  deriving newtype (Eq, Ord, Show)

-- * Special cases

-------------------------

-- ** HTTP special case

-------------------------

{-|
HTTP being by far the most common use-case for resource identifiers,
it's been isolated into a dedicated data-type,
which is optimised for that particular case.

Compared to the general URI definition it:

* only supports the HTTP and HTTPS schemes
* misses the Username and Password components
* requires the Host component
* requires the Path component to be absolute
-}
data HttpIRI
  = HttpIRI Security Host Port Path Query Fragment
  deriving stock (Eq, Ord, Show)

newtype Security
  = Security Bool
  deriving newtype (Eq, Ord, Show)
