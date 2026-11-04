module LordOfTheRings.Enemy.Cards.SquintEyedSoutherner (squintEyedSoutherner) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (eitherRingwraithOr)

newtype SquintEyedSoutherner = SquintEyedSoutherner EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

squintEyedSoutherner :: EnemyCard SquintEyedSoutherner
squintEyedSoutherner = enemy SquintEyedSoutherner Cards.squintEyedSoutherner

-- | When Revealed: You must either shuffle an out of play Ringwraith into the encounter
-- deck or reveal an additional encounter card. "You" is the player who revealed it.
instance RunMessage SquintEyedSoutherner where
  runMessage msg e@(SquintEyedSoutherner attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      eitherRingwraithOr you attrs "squintEyedSoutherner.choose" True "squintEyedSoutherner.reveal" revealEncounterCard
      pure e
    _ -> SquintEyedSoutherner <$> runMessage msg attrs
