module LordOfTheRings.Hero.Cards.Beregond (beregond) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype Beregond = Beregond HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

beregond :: HeroCard Beregond
beregond = hero Beregond Cards.beregond

-- | Lower the cost to play Weapon and Armor attachments on Beregond by 2.
instance HasModifiersFor Beregond where
  getModifiersFor (Beregond a) = modifySelf a [ReduceCostToAttach (CardMatchAny [CardWithTrait Weapon, CardWithTrait Armor]) 2]
