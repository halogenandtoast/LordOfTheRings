module LordOfTheRings.Hero.Cards.Beorn (beorn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Beorn = Beorn HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
beorn :: HeroCard Beorn
beorn = hero Beorn Cards.beorn

-- | Cannot have attachments. Immune to player card effects. Beorn does not exhaust to
-- defend.
instance HasModifiersFor Beorn where
  getModifiersFor (Beorn a) = modifySelf a [CannotHaveAttachments, ImmuneToPlayerCardEffects, DoesNotExhaustToDefend]

instance RunMessage Beorn where
  runMessage msg (Beorn attrs) = Beorn <$> runMessage msg attrs
