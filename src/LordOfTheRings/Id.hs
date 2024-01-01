module LordOfTheRings.Id (module LordOfTheRings.Id) where

import Control.Monad.Random (Random)
import Data.Aeson
import Data.Coerce
import Data.UUID (UUID)
import GHC.Generics
import LordOfTheRings.Prelude

newtype LocationId = LocationId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype EnemyId = EnemyId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype QuestId = QuestId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype HeroId = HeroId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype AllyId = AllyId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype PlayerId = PlayerId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype CardId = CardId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Ord, Eq)

newtype CostId = CostId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Eq)

data RefKind = TargetRef | SourceRef

data AbilityRef = AbilityRef Target Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

isAbility :: (Reference a) => a -> Int -> AbilityRef -> Bool
isAbility a n (AbilityRef t i) = isTarget a t && n == i

data Ref (k :: RefKind)
  = EnemyRef EnemyId
  | LocationRef LocationId
  | QuestRef QuestId
  | HeroRef HeroId
  | AllyRef AllyId
  | PlayerRef PlayerId
  | CardRef CardId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

type Target = Ref 'TargetRef
type Source = Ref 'SourceRef

toTarget :: Source -> Target
toTarget = coerce

toSource :: Target -> Source
toSource = coerce

class Reference a where
  toRef :: a -> Ref k

isTarget :: (Reference a) => a -> Target -> Bool
isTarget a t = toRef a == t
