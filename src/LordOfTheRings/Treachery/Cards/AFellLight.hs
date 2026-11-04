module LordOfTheRings.Treachery.Cards.AFellLight (aFellLight) where

import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (undeadEnemy)
import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AFellLight = AFellLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFellLight :: TreacheryCard AFellLight
aFellLight = treachery AFellLight Cards.aFellLight

-- | Peril.
-- When Revealed: Put the topmost Undead enemy in the encounter discard pile into play
-- engaged with you. Each Undead enemy engaged with you makes an immediate attack.
-- Shadow: Return the topmost Undead enemy in the encounter discard pile to the staging
-- area.
instance RunMessage AFellLight where
  runMessage msg t@(AFellLight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      discardPile <- getEncounterDiscard
      for_ (find (cardMatches undeadEnemy) discardPile) \c -> do
        removeFromEncounterDiscard c
        putEnemyIntoPlayEngaged you c
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      you <- getRevealer
      undead <- select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId you))
      for_ undead (immediateAttack you)
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      discardPile <- getEncounterDiscard
      for_ (find (cardMatches undeadEnemy) discardPile) addToStagingFromDiscard
      pure t
    _ -> AFellLight <$> runMessage msg attrs
