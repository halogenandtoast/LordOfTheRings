module LordOfTheRings.Treachery.Cards.SurprisingSpeed (surprisingSpeed) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SurprisingSpeed = SurprisingSpeed TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

surprisingSpeed :: TreacheryCard SurprisingSpeed
surprisingSpeed = treachery SurprisingSpeed Cards.surprisingSpeed

-- | When Revealed: Each player returns 1 enemy engaged with him to the staging area. If no
-- enemy was returned to the staging area this way, Surprising Speed gains surge and doomed 1.
-- Shadow: Attacking enemy gets +1 attack. Return attacking enemy to the staging area after
-- this attack.
instance RunMessage SurprisingSpeed where
  runMessage msg t@(SurprisingSpeed attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      returned <- for players \p -> do
        engaged <- select (EnemyEngagedWith (PlayerWithId p))
        chooseOrRunOneM p do
          questionLabeled "surprisingSpeed.return"
          causedBy attrs
          targets engaged \e -> placeEnemy e StagingArea
        pure (notNull engaged)
      unless (or returned) do
        raiseThreatFromDoomed 1
        revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      returnToStagingAfterAttack eid
      pure t
    _ -> SurprisingSpeed <$> runMessage msg attrs
