module JSONLD.Types
  ( TermDefinition (..)
  , ActiveContext (..)

    -- ** Term
  , Term
  , mkTerm

    -- ** Compact IRI
  , CompactIRI
  , mkCompactIRI

    -- ** Keyword
  , Keyword
  , fromText

    -- ** Expanded Term Definition
  , ExpandedTermDefinition
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Text
import Data.Vector (Vector)
import IRI.Parser qualified as IRI
import IRI.Types (IRI)

{-| Compact IRI

 /See https://www.w3.org/TR/json-ld11/#compact-iris/
-}
newtype CompactIRI = CompactIRI IRI
  deriving newtype (Eq, Ord, Show)

data TermDefinition
  = TDIRI IRI
  | TDCompact CompactIRI
  | TDTerm Term
  | TDKeyword Keyword
  | TDExpanded ExpandedTermDefinition
  deriving stock (Show, Ord, Eq)

newtype ExpandedTermDefinition = ExpandedTermDefinition (Map Term Text)
  deriving newtype (Eq, Ord, Show)

data EDTParseError
  = -- | The key must be valid
    InvalidKey
  | -- | When the associated term is @type, the expanded term definition
    -- /MUST NOT/ contain keys other than @\@container@ and @\@protected@.
    -- The value of @\@container@ is limited to the single value @\@set@.
    InvalidKeyForAssociatedTermType
  | -- | When the associated term is @\@type@, the expanded term definition
    -- /MUST NOT/ contain keys other than @\@container@ and @\@protected@.
    -- The value of @\@container@ is limited to the single value @\@set@.
    IdKeyNotIncluded
  | -- | If the expanded term definition contains the @\@id@ keyword,
    --  its value /MUST/ be null, an IRI, a blank node identifier, a compact IRI, a term, or a keyword.
    InvalidIdKeywordValue
  | -- | If an expanded term definition has an @\@reverse@ entry,
    -- it /MUST NOT/ have @\@id@ or @\@nest@ entries at the same time,
    -- its value /MUST/ be an IRI, a blank node identifier, a compact IRI, or a term.
    -- If an @\@container@ entry exists, its value /MUST/ be null, @\@set@, or @\@index@.
    InvalidReverseKeywordEntry
  | -- | If the expanded term definition contains the @\@type@ keyword, its value MUST be an IRI,
    -- a compact IRI, a term, null, or one of the keywords @\@id@, @\@json@, @\@none@, or @\@vocab@.
    InvalidTypeKeywordEntry
  | -- | If the expanded term definition contains the @\@language@ keyword,
    -- its value MUST have the lexical form described in BCP47 or be null.
    InvalidLanguageKeywordEntry
  | -- | If the expanded term definition contains the @\@index@ keyword, its value MUST be an IRI, a compact IRI, or a term.
    InvalidIndexKeywordEntry
  | -- | If the expanded term definition contains the @\@container@ keyword, its value /MUST/ be either
    -- @\@list@, @\@set@, @\@language@, @\@index@, @\@id@, @\@graph@, @\@type@, or be null or
    -- an array containing exactly any one of those keywords, or a combination of @\@set@ and any of
    -- @\@index@, @\@id@, @\@graph@, @\@type@, @\@language@ in any order. @\@container@ may also be an array
    -- containing @\@graph@ along with either @\@id@ or @\@index@ and also optionally including @\@set@.
    -- If the value is @\@language@, when the term is used outside of the @\@context@, the associated value
    -- /MUST/ be a language map. If the value is @\@index@, when the term is used outside
    -- of the @\@context@, the associated value /MUST/ be an index map.
    InvalidContainerKeywordEntry
  | -- | If an expanded term definition has an \@context entry, it MUST be a valid context definition.
    InvalidContextKeywordEntry
  | -- | If the expanded term definition contains the @nest keyword, its value MUST be either @nest, or a term which expands to @nest.
    InvalidNestKeywordEntry
  | -- |If the expanded term definition contains the @prefix keyword, its value MUST be true or false.
    InvalidPrefixKeywordEntry
  | -- |If the expanded term definition contains the @propagate keyword, its value MUST be true or false.
    InvalidPropagateKeywordEntry
  | -- |If the expanded term definition contains the @protected keyword, its value MUST be true or false.
    InvalidProtectedKeywordEntry
  deriving stock (Show, Ord, Eq)

data Keyword
  = -- \^ `@base`.
    -- Used to set the base IRI against which to resolve those relative IRI references
    -- which are otherwise interpreted relative to the document.
    Base
  | -- | `@context`.
    -- Used to define the short-hand names that are used throughout a JSON-LD document.
    Container
  | -- | `@container`.
    -- Used to set the default container type for a term.
    Context
  | -- | `@direction`.
    -- Used to set the base direction of a JSON-LD value, which are not typed values.
    -- (e.g. strings, or language-tagged strings).
    Direction
  | -- | `@graph`.
    -- Used to express a graph.
    Graph
  | -- | `@id`.
    -- Used to uniquely identify node objects that are being described in the document with IRIs
    -- or blank node identifiers.
    Id
  | -- | `@import`.
    -- Used in a context definition to load an external context within which the containing
    -- context definition is merged.
    Import
  | -- | `@included`.
    -- Used in a top-level node object to define an included block, for including secondary node
    -- objects within another node object.
    Included
  | -- | `@index`.
    -- Used to specify that a container is used to index information and that processing should
    -- continue deeper into a JSON data structure.
    Index
  | -- | `@json`.
    -- Used as the @type value of a JSON literal.
    Json
  | -- | `@language`.
    -- Used to specify the language for a particular string value or the default language of a
    -- JSON-LD document.
    Language
  | -- | `@list`.
    -- Used to express an ordered set of data.
    List
  | -- `@nest`.
    -- Used to define a property of a node object that groups together properties of that node,
    -- but is not an edge in the graph.
    Nest
  | -- `@none`.
    -- Used as an index value in an index map, id map, language map, type map, or elsewhere where
    -- a map is used to index into other values, when the indexed node does not have the feature
    -- being indexed.
    None
  | -- `@prefix`.
    -- With the value true, allows this term to be used to construct a compact IRI when
    -- compacting.
    Prefix
  | -- `@propagate`.
    -- Used in a context definition to change the scope of that context.
    --
    -- By default, it is true, meaning that contexts propagate across node objects
    -- (other than for type-scoped contexts, which default to false).
    -- Setting this to false causes term definitions created within that context to be removed
    -- when entering a new node object.
    Propagate
  | -- `@protected`.
    -- Used to prevent term definitions of a context to be overridden by other contexts.
    Protected
  | -- `@reverse`.
    -- Used to express reverse properties.
    Reverse
  | -- `@set`.
    -- Used to express an unordered set of data and to ensure that values are always represented
    -- as arrays.
    Set
  | -- `@type`.
    -- Used to set the type of a node or the datatype of a typed value.
    Type
  | -- `@value`.
    -- Used to specify the data that is associated with a particular property in the graph.
    Value
  | -- `@version`.
    -- Used in a context definition to set the processing mode.
    Version
  | -- `@vocab`.
    -- Used to expand properties and values in @type with a common prefix IRI.
    Vocab
  deriving stock (Eq, Ord, Show)

instance Display Keyword where
  displayBuilder kw = "@" <> Text.fromText (Text.toLower $ Text.pack $ show kw)

fromText :: Text -> Maybe Keyword
fromText = \case
  "@base" -> Just Base
  "@container" -> Just Container
  "@context" -> Just Context
  "@direction" -> Just Direction
  "@graph" -> Just Graph
  "@id" -> Just Id
  "@import" -> Just Import
  "@included" -> Just Included
  "@index" -> Just Index
  "@json" -> Just Json
  "@language" -> Just Language
  "@list" -> Just List
  "@nest" -> Just Nest
  "@none" -> Just None
  "@prefix" -> Just Prefix
  "@propagate" -> Just Propagate
  "@protected" -> Just Protected
  "@reverse" -> Just Reverse
  "@set" -> Just Set
  "@type" -> Just Type
  "@value" -> Just Value
  "@version" -> Just Version
  "@vocab" -> Just Vocab
  _ -> Nothing

data ActiveContext = ActiveContext
  { termDefinitions :: Vector TermDefinition
  }

data CompactIRIParseError
  = NoColon
  | BeginsWithTwoSlashes
  | BlankNodeIdentifier
  | UnparsableIRI Text
  deriving stock (Eq, Ord, Show)

data TermParseError
  = EmptyString
  | HasColon
  | KeywordConflict
  deriving stock (Eq, Ord, Show)

mkCompactIRI :: Text -> Either CompactIRIParseError CompactIRI
mkCompactIRI text =
  hasColon text
    >>= suffixDoesNotBeginWithTwoSlashes
    >>= isNotABlankNodeIdentifier
    >>= parseCompactIRI

-- | Returns either a parse error or the @(prefix, suffix)@ tuple.
hasColon :: Text -> Either CompactIRIParseError (Text, Text)
hasColon text =
  if Text.isInfixOf ":" text
    then Right $ Text.breakOn ":" text
    else Left NoColon

suffixDoesNotBeginWithTwoSlashes :: (Text, Text) -> Either CompactIRIParseError (Text, Text)
suffixDoesNotBeginWithTwoSlashes (_, suffix) | Text.isPrefixOf "//" suffix = Left BeginsWithTwoSlashes
suffixDoesNotBeginWithTwoSlashes pair = Right pair

isNotABlankNodeIdentifier :: (Text, Text) -> Either CompactIRIParseError (Text, Text)
isNotABlankNodeIdentifier ("_", _) = Left BlankNodeIdentifier
isNotABlankNodeIdentifier (prefix, suffix) = Right (prefix, suffix)

parseCompactIRI :: (Text, Text) -> Either CompactIRIParseError CompactIRI
parseCompactIRI (prefix, suffix) =
  case IRI.parseIRI (prefix <> ":" <> suffix) of
    Left msg -> Left $ UnparsableIRI msg
    Right iri -> Right $ CompactIRI iri

newtype Term = Term Text
  deriving newtype (Eq, Ord, Show)

mkTerm :: Text -> Either TermParseError Term
mkTerm text =
  isTextEmpty text
    >>= hasNoColon
    >>= hasKeywordConflict

isTextEmpty :: Text -> Either TermParseError Text
isTextEmpty text | Text.null text = Left EmptyString
isTextEmpty text = Right text

hasNoColon :: Text -> Either TermParseError Text
hasNoColon text | Text.isInfixOf ":" text = Left HasColon
hasNoColon text = Right text

hasKeywordConflict :: Text -> Either TermParseError Term
hasKeywordConflict text | Text.isPrefixOf "@" text = Left KeywordConflict
hasKeywordConflict text = Right (Term text)
