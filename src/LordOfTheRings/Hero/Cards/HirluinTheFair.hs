module LordOfTheRings.Hero.Cards.HirluinTheFair (hirluinTheFair) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Outlands))

newtype HirluinTheFair = HirluinTheFair HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hirluinTheFair :: HeroCard HirluinTheFair
hirluinTheFair = hero HirluinTheFair Cards.hirluinTheFair

-- | You may use resources from Hirluin the Fair's resource pool to pay for Outlands ally
-- cards of any sphere.
instance HasModifiersFor HirluinTheFair where
  getModifiersFor (HirluinTheFair a) =
    modifySelf a [GainResourceIconFor (CardWithType AllyType <> CardWithTrait Outlands) s | s <- [Leadership, Tactics, Spirit, Lore]]
