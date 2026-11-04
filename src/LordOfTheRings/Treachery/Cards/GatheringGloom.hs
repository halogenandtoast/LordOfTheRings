module LordOfTheRings.Treachery.Cards.GatheringGloom (gatheringGloom) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GatheringGloom = GatheringGloom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gatheringGloom :: TreacheryCard GatheringGloom
gatheringGloom = treachery GatheringGloom Cards.gatheringGloom

-- | Doomed 1.
-- When Revealed: Each player must choose: either raise your threat by 1 for each ally you
-- control, or reveal an encounter card.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, discard an
-- ally you control.
instance RunMessage GatheringGloom where
  runMessage msg t@(GatheringGloom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- length <$> select (AllyControlledBy $ PlayerWithId p)
        chooseOneM p do
          questionLabeled "gatheringGloom.choose"
          causedBy attrs
          labeledWith "gatheringGloom.raiseThreat" [intVar "n" allies] $ encounterRaisesThreat p allies
          labeled "gatheringGloom.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "gatheringGloom.discardAlly"
        causedBy attrs
        targets allies discard
      pure t
    _ -> GatheringGloom <$> runMessage msg attrs
