module LordOfTheRings.Hero.Cards.Amarthiul (amarthiul) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype Amarthiul = Amarthiul HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

amarthiul :: HeroCard Amarthiul
amarthiul = hero Amarthiul Cards.amarthiul

-- | While you are engaged with at least 1 enemy, Amarthiúl gains the tactics resource icon.
-- While you are engaged with at least 2 enemies, add 1 additional resource to Amarthiúl's
-- resource pool when you collect resources during the resource phase.
instance HasModifiersFor Amarthiul where
  getModifiersFor (Amarthiul a) = do
    engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId a.owner)
    modifySelf a $ [GainResourceIcon Tactics | engaged >= 1] <> [CollectsAdditionalResources 1 | engaged >= 2]
