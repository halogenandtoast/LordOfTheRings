module LordOfTheRings.Treachery.Cards.SarumansOrders (sarumansOrders) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SarumansOrders = SarumansOrders TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sarumansOrders :: TreacheryCard SarumansOrders
sarumansOrders = treachery SarumansOrders Cards.sarumansOrders

{- | When Revealed: Each player must choose: either return an enemy engaged with him to the
staging area, or reveal an encounter card. A player with no enemy engaged reveals one; the
card he reveals is his, "you" on it reading him (The Black Riders rules insert, "New
Staging Rules").
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, return
attacking enemy to the staging area.
-}
instance RunMessage SarumansOrders where
  runMessage msg t@(SarumansOrders attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "sarumansOrders.choose"
          causedBy attrs
          targets engaged \e -> placeEnemy e StagingArea
          labeled "sarumansOrders.reveal" $ asRevealer p revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      stillHere <- selectAny (EnemyWithId eid <> not_ EnemyInStagingArea)
      when stillHere $ placeEnemy eid StagingArea
      pure t
    _ -> SarumansOrders <$> runMessage msg attrs
