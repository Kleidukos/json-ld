module JSONLD.ContextProcessing where

import Data.Map (Map)
import JSONLD.Types (Term)

data ActiveContext = ActiveContext
  { 

  }

newtype InverseContext = InverseContext (Map Term InverseDefinition)
  deriving newtype (Eq, Ord, Show)

newtype InverseDefinition = InverseDefinition (Map Container InverseContainer)
  deriving newtype (Eq, Ord, Show)

newtype InverseContainer = InverseContainer
  deriving newtype (Eq, Ord, Show)
newtype Container = Container
  deriving newtype (Eq, Ord, Show)
