module LordOfTheRings.Treachery.Cards.TakenPrisoner (takenPrisoner) where

import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (captiveOfStaged, unattachedInStaging)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TakenPrisoner = TakenPrisoner TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

takenPrisoner :: TreacheryCard TakenPrisoner
takenPrisoner = treachery TakenPrisoner Cards.takenPrisoner

-- | When Revealed: The first player attaches an ally he controls to an unattached enemy or
-- location in the staging area. If he cannot, Taken Prisoner gains doomed 2.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys an ally, reveal the top
-- card of the encounter deck.
instance RunMessage TakenPrisoner where
  runMessage msg t@(TakenPrisoner attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      allies <- select (AllyControlledBy $ PlayerWithId lead)
      captors <- unattachedInStaging True
      if null allies || null captors
        then raiseThreatFromDoomed 2
        else chooseOrRunOneM lead do
          questionLabeled "takenPrisoner.ally"
          causedBy attrs
          targets allies \al -> chooseOrRunOneM lead do
            questionLabeled "theDungeonsOfCirithGurat.attachCaptive"
            causedBy attrs
            targets captors (captiveOfStaged al)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      allies <- attackDestroyedAllies eid
      when (notNull allies) revealEncounterCard
      pure t
    _ -> TakenPrisoner <$> runMessage msg attrs
