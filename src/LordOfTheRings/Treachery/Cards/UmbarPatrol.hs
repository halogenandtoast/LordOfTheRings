module LordOfTheRings.Treachery.Cards.UmbarPatrol (umbarPatrol) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UmbarPatrol = UmbarPatrol TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarPatrol :: TreacheryCard UmbarPatrol
umbarPatrol = treachery UmbarPatrol Cards.umbarPatrol

-- | When Revealed: Each player returns an enemy engaged with him to the staging area.
-- Until the end of the round, enemies cannot be optionally engaged. If there are no
-- enemies in the staging area, search the encounter deck and discard pile for an enemy and
-- add it to the staging area.
instance RunMessage UmbarPatrol where
  runMessage msg t@(UmbarPatrol attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "umbarPatrol.return"
          causedBy attrs
          targets engaged \e -> placeEnemy e StagingArea
      roundModifier attrs GameTarget (CannotBeOptionallyEngagedBy AnyPlayer)
      doStep 1 msg
      pure t
    -- the staging area looked at once the enemies have returned to it
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      none <- not <$> selectAny EnemyInStagingArea
      when none do
        lead <- getLead
        searchEncounterDeckAndDiscard lead attrs (CardWithType EnemyType)
      pure t
    _ -> UmbarPatrol <$> runMessage msg attrs
