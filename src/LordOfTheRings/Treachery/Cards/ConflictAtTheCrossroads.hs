module LordOfTheRings.Treachery.Cards.ConflictAtTheCrossroads (conflictAtTheCrossroads) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)
import LordOfTheRings.Trait (Trait (Orc))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ConflictAtTheCrossroads = ConflictAtTheCrossroads TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

conflictAtTheCrossroads :: TreacheryCard ConflictAtTheCrossroads
conflictAtTheCrossroads = treachery ConflictAtTheCrossroads Cards.conflictAtTheCrossroads

-- | When Revealed: Until the end of the phase, Orc enemies engaged with players add their
-- threat to the staging area. If no Orc enemies are engaged with players, Conflict at the
-- Crossroads gains surge.
-- Shadow: Take 1 hidden card.
instance RunMessage ConflictAtTheCrossroads where
  runMessage msg t@(ConflictAtTheCrossroads attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      phaseModifier attrs GameTarget (EngagedEnemiesAddThreat $ EnemyWithTrait Orc)
      whenM (selectNone $ EnemyWithTrait Orc <> EnemyEngagedWith AnyPlayer) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      takeHiddenCard pid
      pure t
    _ -> ConflictAtTheCrossroads <$> runMessage msg attrs
