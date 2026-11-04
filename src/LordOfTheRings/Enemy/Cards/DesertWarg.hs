module LordOfTheRings.Enemy.Cards.DesertWarg (desertWarg) where

import LordOfTheRings.EncounterSet (EncounterSet (DesertCreatures))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (discardRandomSetAsideEnemyLabeled)

newtype DesertWarg = DesertWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desertWarg :: EnemyCard DesertWarg
desertWarg = enemy DesertWarg Cards.desertWarg

{- | When Revealed: Either place a random enemy from the set aside Desert Creatures set into
the encounter discard pile, or Desert Warg gains surge. The first player decides.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage DesertWarg where
  runMessage msg e@(DesertWarg attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "desertWarg.choose"
        causedBy attrs
        discardRandomSetAsideEnemyLabeled "desertWarg.discard" DesertCreatures
        labeled "desertWarg.surge" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> DesertWarg <$> runMessage msg attrs
