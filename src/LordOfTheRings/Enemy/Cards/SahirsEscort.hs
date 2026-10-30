module LordOfTheRings.Enemy.Cards.SahirsEscort (sahirsEscort) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (addToStormcallerArea)

newtype SahirsEscort = SahirsEscort EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsEscort :: EnemyCard SahirsEscort
sahirsEscort = enemy SahirsEscort Cards.sahirsEscort

-- | Boarding 1. Cannot have attachments.
-- While Sahír's Escort is in the Stormcaller's area, it contributes its threat to the
-- amount of progress the Stormcaller places on its quest stage each round: the scenario
-- counts it as it places that progress.
instance HasModifiersFor SahirsEscort where
  getModifiersFor (SahirsEscort a) = modifySelf a [CannotHaveAttachments]

-- | When Revealed: Add Sahír's Escort to the Stormcaller's area, instead of the players'
-- staging area.
instance RunMessage SahirsEscort where
  runMessage msg e@(SahirsEscort attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addToStormcallerArea (toTarget attrs)
      pure e
    _ -> SahirsEscort <$> runMessage msg attrs
