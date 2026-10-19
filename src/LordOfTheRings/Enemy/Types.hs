{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Enemy.Types (module LordOfTheRings.Enemy.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Name
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ EnemyId
  , EntityAttrs a ~ EnemyAttrs
  )
  => IsEnemy a

type EnemyCard a = CardBuilder EnemyId a

data Enemy = forall a. (IsEnemy a) => Enemy a

data SomeEnemyCard = forall a. (IsEnemy a) => SomeEnemyCard (EnemyCard a)

data EnemyAttrs = EnemyAttrs
  { enemyId :: EnemyId
  , enemyCardId :: CardId
  , enemyCardCode :: CardCode
  , enemyPlacement :: Placement
  , enemyTokens :: Tokens
  , enemyShadowCards :: [EncounterCard]
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''EnemyAttrs

instance ToJSON EnemyAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "enemy")

enemy :: (EnemyAttrs -> a) -> CardDef -> EnemyCard a
enemy f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId ident ->
        f
          EnemyAttrs
            { enemyId = ident
            , enemyCardId = cardId
            , enemyCardCode = cdCardCode cardDef
            , enemyPlacement = Unplaced
            , enemyTokens = mempty
            , enemyShadowCards = []
            }
    }

instance Entity EnemyAttrs where
  type EntityId EnemyAttrs = EnemyId
  type EntityAttrs EnemyAttrs = EnemyAttrs
  toId = enemyId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance HasModifiersFor Enemy where
  getModifiersFor (Enemy a) = getModifiersFor a

instance HasAbilities Enemy where
  getAbilities (Enemy a) = getAbilities a

instance Show Enemy where
  show (Enemy a) = show a

instance ToJSON Enemy where
  toJSON (Enemy a) = toJSON a

instance IsCard EnemyAttrs where
  toCard a = EncounterCard $ MkEncounterCard {ecId = enemyCardId a, ecCardCode = enemyCardCode a}

instance IsCard Enemy where
  toCard = toCard . toAttrs

instance HasCardCode EnemyAttrs where
  toCardCode = enemyCardCode

instance HasCardCode Enemy where
  toCardCode = toCardCode . toAttrs

instance HasCardDef EnemyAttrs where
  toCardDef = toCardDef . enemyCardCode

instance HasCardDef Enemy where
  toCardDef = toCardDef . toAttrs

instance Targetable EnemyAttrs where
  toTarget = EnemyTarget . enemyId

instance Targetable Enemy where
  toTarget = toTarget . toAttrs

instance Sourceable EnemyAttrs where
  toSource = EnemySource . enemyId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" EnemyAttrs EnemyId where
  getField = enemyId

instance HasField "id" Enemy EnemyId where
  getField = toId

instance HasField "placement" EnemyAttrs Placement where
  getField = enemyPlacement

instance HasField "placement" Enemy Placement where
  getField = (.placement) . toAttrs

instance HasField "name" EnemyAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Enemy Name where
  getField = (.name) . toAttrs

instance HasField "engagementCost" EnemyAttrs Int where
  getField = fromMaybe 0 . cdEngagementCost . toCardDef

instance HasField "engagementCost" Enemy Int where
  getField = (.engagementCost) . toAttrs

instance HasField "threat" EnemyAttrs Int where
  getField = fromMaybe 0 . cdThreat . toCardDef

instance HasField "threat" Enemy Int where
  getField = (.threat) . toAttrs

instance HasField "attack" EnemyAttrs Int where
  getField = fromMaybe 0 . cdAttack . toCardDef

instance HasField "attack" Enemy Int where
  getField = (.attack) . toAttrs

instance HasField "defense" EnemyAttrs Int where
  getField = fromMaybe 0 . cdDefense . toCardDef

instance HasField "defense" Enemy Int where
  getField = (.defense) . toAttrs

instance HasField "hitPoints" EnemyAttrs Int where
  getField = fromMaybe 0 . cdHitPoints . toCardDef

instance HasField "hitPoints" Enemy Int where
  getField = (.hitPoints) . toAttrs

instance HasField "damage" EnemyAttrs Int where
  getField = countTokens Damage . enemyTokens

instance HasField "damage" Enemy Int where
  getField = (.damage) . toAttrs

instance HasField "shadowCards" EnemyAttrs [EncounterCard] where
  getField = enemyShadowCards

instance HasField "shadowCards" Enemy [EncounterCard] where
  getField = (.shadowCards) . toAttrs

instance HasField "cardId" EnemyAttrs CardId where
  getField = enemyCardId
