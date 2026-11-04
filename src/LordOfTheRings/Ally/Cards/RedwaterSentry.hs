module LordOfTheRings.Ally.Cards.RedwaterSentry (redwaterSentry) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Trait (Trait (Armor))

newtype RedwaterSentry = RedwaterSentry AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

redwaterSentry :: AllyCard RedwaterSentry
redwaterSentry = ally RedwaterSentry Cards.redwaterSentry

-- | Reduce the cost to play Armor attachments on Redwater Sentry by 1. Redwater Sentry gets
-- +1 defense and gains Sentinel while it has an Armor attachment.
instance HasModifiersFor RedwaterSentry where
  getModifiersFor (RedwaterSentry a) = do
    armored <- notNull <$> attachedWithTrait Armor (toTarget a)
    modifySelf a $ ReduceCostToAttach (CardWithTrait Armor) 1 : [m | armored, m <- [DefenseModifier 1, GainKeyword Sentinel]]
