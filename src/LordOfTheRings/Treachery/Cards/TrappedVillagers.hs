module LordOfTheRings.Treachery.Cards.TrappedVillagers (trappedVillagers) where

import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (objectiveOf, villagerDies)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TrappedVillagers = TrappedVillagers TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trappedVillagers :: TreacheryCard TrappedVillagers
trappedVillagers = treachery TrappedVillagers Cards.trappedVillagers

-- | When Revealed: Place 3 damage tokens on Dead Villagers. Until the end of the round, add
-- 1 to the total threat in the staging area for each damage token on Dead Villagers.
-- Shadow: Attacking enemy gets +2 attack. If this attack destroys an ally, place 1 damage
-- token on Dead Villagers.
instance RunMessage TrappedVillagers where
  runMessage msg t@(TrappedVillagers attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      replicateM_ 3 $ villagerDies attrs
      doStep 1 msg
      pure t
    -- counted as the damage on Dead Villagers changes this round (FAQ 1.55)
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      objectiveOf Objectives.deadVillagers >>= traverse_ \o -> roundModifier attrs GameTarget (StagingThreatPerToken (ObjectiveTarget o) Damage)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      afterAttack eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      allies <- attackDestroyedAllies eid
      unless (null allies) $ villagerDies attrs
      pure t
    _ -> TrappedVillagers <$> runMessage msg attrs
