{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Hero.Types (module LordOfTheRings.Hero.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Name
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
  , EntityId a ~ HeroId
  , EntityAttrs a ~ HeroAttrs
  )
  => IsHero a

type HeroCard a = CardBuilder (HeroId, PlayerId) a

data Hero = forall a. (IsHero a) => Hero a

data SomeHeroCard = forall a. (IsHero a) => SomeHeroCard (HeroCard a)

data HeroAttrs = HeroAttrs
  { heroId :: HeroId
  , heroCardId :: CardId
  , heroCardCode :: CardCode
  , heroOwner :: PlayerId
  , heroTokens :: Tokens
  , heroExhausted :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''HeroAttrs

instance ToJSON HeroAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "hero")

hero :: (HeroAttrs -> a) -> CardDef -> HeroCard a
hero f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId (hid, pid) ->
        f
          HeroAttrs
            { heroId = hid
            , heroCardId = cardId
            , heroCardCode = cdCardCode cardDef
            , heroOwner = pid
            , heroTokens = mempty
            , heroExhausted = False
            }
    }

instance Entity HeroAttrs where
  type EntityId HeroAttrs = HeroId
  type EntityAttrs HeroAttrs = HeroAttrs
  toId = heroId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Hero where
  type EntityId Hero = HeroId
  type EntityAttrs Hero = HeroAttrs
  toId = toId . toAttrs
  toAttrs (Hero a) = toAttrs a
  overAttrs f (Hero a) = Hero $ overAttrs f a

instance HasModifiersFor Hero where
  getModifiersFor (Hero a) = getModifiersFor a

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a

instance Show Hero where
  show (Hero a) = show a

instance ToJSON Hero where
  toJSON (Hero a) = toJSON a

instance HasCardCode HeroAttrs where
  toCardCode = heroCardCode

instance HasCardCode Hero where
  toCardCode = toCardCode . toAttrs

instance HasCardDef HeroAttrs where
  toCardDef = toCardDef . heroCardCode

instance HasCardDef Hero where
  toCardDef = toCardDef . toAttrs

instance Targetable HeroAttrs where
  toTarget = HeroTarget . heroId

instance Targetable Hero where
  toTarget = toTarget . toAttrs

instance Sourceable HeroAttrs where
  toSource = HeroSource . heroId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Hero where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" HeroAttrs HeroId where
  getField = heroId

instance HasField "owner" HeroAttrs PlayerId where
  getField = heroOwner

instance HasField "name" HeroAttrs Name where
  getField = cdName . toCardDef

instance HasField "sphere" HeroAttrs Sphere where
  getField = fromMaybe Neutral . cdSphere . toCardDef

instance HasField "threat" HeroAttrs Int where
  getField = fromMaybe 0 . cdThreatCost . toCardDef

instance HasField "resources" HeroAttrs Int where
  getField = countTokens Resource . heroTokens

instance HasField "id" Hero HeroId where
  getField = toId

instance HasField "owner" Hero PlayerId where
  getField = (.owner) . toAttrs

instance HasField "name" Hero Name where
  getField = (.name) . toAttrs

instance HasField "sphere" Hero Sphere where
  getField = (.sphere) . toAttrs

instance HasField "threat" Hero Int where
  getField = (.threat) . toAttrs

instance HasField "resources" Hero Int where
  getField = (.resources) . toAttrs
