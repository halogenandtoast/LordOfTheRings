module LordOfTheRings.Enemy.Cards.FramsShade (framsShade) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootControlled)

newtype FramsShade = FramsShade EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

framsShade :: EnemyCard FramsShade
framsShade = enemy FramsShade Cards.framsShade

-- | Cannot have non-objective attachments. Fram's Shade gets +1 threat, +1 attack, and +1
-- defense for each Loot objective the players control. When Fram's Shade is dealt shadow
-- cards, deal it a number of shadow cards equal to the number of Loot objectives the
-- players control (to a minimum of 1).
-- An objective guarding with it is not an attachment card, so it still may.
instance HasModifiersFor FramsShade where
  getModifiersFor (FramsShade a) = do
    loot <- lootControlled
    modifySelf a
      $ [CannotHaveAttachments, ShadowCardsDealt (max 1 loot)]
      <> (guard (loot > 0) *> [ThreatModifier loot, AttackModifier loot, DefenseModifier loot])

instance RunMessage FramsShade where
  runMessage msg (FramsShade attrs) = FramsShade <$> runMessage msg attrs
