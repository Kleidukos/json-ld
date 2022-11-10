{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module IRI.Parser (parseIRI, urlEncodedComponentText) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate, try)
import Control.Monad (MonadPlus, join, mplus)
import Data.Attoparsec.Text hiding (try)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (ord)
import Data.Functor (($>))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word16, Word8)
import Net.IPv4 qualified as IPv4
import Net.IPv6 qualified as IPv6
import Ptr.ByteString qualified as ByteString
import Ptr.Poking qualified as Poking
import System.IO.Unsafe (unsafeDupablePerformIO)
import Text.Builder qualified as TB
import VectorBuilder.MonadPlus as Vector
import Prelude hiding (foldl)

import Data.Attoparsec.Text qualified as Attoparsec
import IRI.CodePoint.RFC3987 qualified as RFC3987
import IRI.Types

-- | Parse an RFC-compliant IRI
parseIRI :: Text -> Either Text IRI
parseIRI =
  either (Left . fromString) Right
    . Attoparsec.parseOnly (iri <* Attoparsec.endOfInput)

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label

{-|
Parser of a well-formed IRI conforming to the RFC3987 standard into 'IRI'.
Performs URL-decoding.
-}
{-# INLINEABLE iri #-}
iri :: Parser IRI
iri =
  labeled "IRI" $ do
    scheme <- parseScheme
    char ':'
    hierarchy <- parseHierarchy
    query <- parseQuery
    fragment <- parseFragment
    pure IRI{..}

{-# INLINE parseHierarchy #-}
parseHierarchy :: Parser Hierarchy
parseHierarchy =
  do
    slashPresent <- char '/' $> True <|> pure False
    if slashPresent
      then do
        slashPresent' <- char '/' $> True <|> pure False
        if slashPresent'
          then authorisedHierarchyBody AuthorisedHierarchy
          else AbsoluteHierarchy <$> path
      else RelativeHierarchy <$> path

{-# INLINE authorisedHierarchyBody #-}
authorisedHierarchyBody :: (Authority -> Path -> body) -> Parser body
authorisedHierarchyBody body =
  do
    parsedUserInfo <- presentUserInfo PresentUserInfo <* char '@' <|> pure MissingUserInfo
    parsedHost <- host
    parsedPort <- PresentPort <$> (char ':' *> port) <|> pure MissingPort
    parsedPath <- char '/' *> path <|> pure (Path mempty)
    pure (body (Authority parsedUserInfo parsedHost parsedPort) parsedPath)

{-# INLINE parseScheme #-}
parseScheme :: Parser Scheme
parseScheme =
  labeled "Scheme" $
    fmap (Scheme . Text.encodeUtf8) (takeWhile1 (RFC3987.scheme . ord))

{-# INLINEABLE presentUserInfo #-}
presentUserInfo :: (User -> Password -> a) -> Parser a
presentUserInfo result =
  labeled "User info" $
    do
      user <- User <$> urlEncodedComponent (RFC3987.unencodedUserInfoComponent . ord)
      passwordFollows <- True <$ char ':' <|> pure False
      if passwordFollows
        then do
          password <- PresentPassword <$> urlEncodedComponent (RFC3987.unencodedUserInfoComponent . ord)
          pure (result user password)
        else pure (result user MissingPassword)

{-# INLINE host #-}
host :: Parser Host
host =
  labeled "Host" $
    IpV6Host <$> IPv6.parser
      <|> IpV4Host <$> IPv4.parser
      <|> NamedHost <$> domainName

{-# INLINE domainName #-}
domainName :: Parser RegName
domainName =
  fmap RegName (Vector.sepBy1 domainLabel (char '.'))

{-|
Domain label with Punycode decoding applied.
-}
{-# INLINE domainLabel #-}
domainLabel :: Parser DomainLabel
domainLabel =
  labeled "Domain label" $
    DomainLabel <$> takeWhile1 (RFC3987.unencodedRegName . ord)

{-# INLINE port #-}
port :: Parser Word16
port =
  decimal

{-# INLINE path #-}
path :: Parser Path
path =
  do
    segments <- Vector.sepBy pathSegment (char '/')
    if segmentsAreEmpty segments
      then pure (Path mempty)
      else pure (Path segments)
  where
    segmentsAreEmpty segments =
      Vector.length segments == 1
        && (case Vector.unsafeHead segments of PathSegment headSegment -> ByteString.null headSegment)

{-# INLINE pathSegment #-}
pathSegment :: Parser PathSegment
pathSegment =
  fmap PathSegment (urlEncodedComponent (RFC3987.unencodedPathSegment . ord))

{-# INLINEABLE urlEncodedComponent #-}
urlEncodedComponent :: (Char -> Bool) -> Parser ByteString
urlEncodedComponent unencodedCharPredicate =
  labeled "URL-encoded component" $
    fmap ByteString.poking $
      fold $
        Poking.bytes . Text.encodeUtf8 <$> takeWhile1 unencodedCharPredicate <|> Poking.word8 <$> urlEncodedByte

{-# INLINEABLE urlEncodedComponentText #-}
urlEncodedComponentText :: (Char -> Bool) -> Parser Text
urlEncodedComponentText unencodedCharPredicate =
  labeled "URL-encoded component" $
    fmap TB.run $
      foldl mappend mempty $
        TB.text <$> takeWhile1 unencodedCharPredicate <|> urlEncodedSequenceTextBuilder

{-# INLINEABLE urlEncodedSequenceTextBuilder #-}
urlEncodedSequenceTextBuilder :: Parser TB.Builder
urlEncodedSequenceTextBuilder =
  labeled "URL-encoded sequence" $ do
    start <- progress (mempty, mempty, Text.streamDecodeUtf8) =<< urlEncodedByte
    foldlM progress start urlEncodedByte >>= finish
  where
    progress (!builder, _ :: ByteString, decode) byte =
      case unsafeDupablePerformIO (try (evaluate (decode (ByteString.singleton byte)))) of
        Right (Text.Some decodedChunk undecodedBytes newDecode) ->
          pure (builder <> TB.text decodedChunk, undecodedBytes, newDecode)
        Left (Text.DecodeError err _) ->
          fail (showString "UTF8 decoding: " err)
    finish (builder, undecodedBytes, _) =
      if ByteString.null undecodedBytes
        then pure builder
        else fail (showString "UTF8 decoding: Bytes remaining: " (show undecodedBytes))

{-# INLINE urlEncodedByte #-}
urlEncodedByte :: Parser Word8
urlEncodedByte =
  do
    char '%'
    digit1 <- fromIntegral <$> hexadecimalDigit
    digit2 <- fromIntegral <$> hexadecimalDigit
    pure (shiftL digit1 4 .|. digit2)

{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Parser Int
hexadecimalDigit =
  do
    c <- anyChar
    let x = ord c
    if x >= 48 && x < 58
      then pure (x - 48)
      else
        if x >= 65 && x < 71
          then pure (x - 55)
          else
            if x >= 97 && x < 103
              then pure (x - 97)
              else fail ("Not a hexadecimal digit: " <> show c)

{-# INLINEABLE parseQuery #-}
parseQuery :: Parser Query
parseQuery =
  labeled "Query" $
    char '?' *> queryBody <|> pure (Query mempty)

{-|
The stuff after the question mark.
-}
{-# INLINEABLE queryBody #-}
queryBody :: Parser Query
queryBody =
  fmap Query (urlEncodedComponent (RFC3987.unencodedQuery . ord))

{-# INLINEABLE parseFragment #-}
parseFragment :: Parser Fragment
parseFragment =
  labeled "Fragment" $
    char '#' *> (Fragment <$> urlEncodedComponent (RFC3987.unencodedFragment . ord))
      <|> pure (Fragment mempty)

{-# INLINE foldl #-}
foldl :: MonadPlus m => (a -> b -> a) -> a -> m b -> m a
foldl step start elementParser =
  loop start
  where
    loop state =
      mplus
        ( do
            element <- elementParser
            loop $! step state element
        )
        (return state)

{-# INLINE foldlM #-}
foldlM :: MonadPlus m => (a -> b -> m a) -> a -> m b -> m a
foldlM step start elementParser =
  loop start
  where
    loop state =
      join
        ( mplus
            ( do
                element <- elementParser
                return (step state element >>= loop)
            )
            (return (return state))
        )

{-# INLINE fold #-}
fold :: (MonadPlus m, Monoid a) => m a -> m a
fold = foldl mappend mempty
