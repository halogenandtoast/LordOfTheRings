module LordOfTheRings.Treachery.Cards.BattleHardened (battleHardened) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype BattleHardened = BattleHardened TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleHardened :: TreacheryCard BattleHardened
battleHardened = treachery BattleHardened Cards.battleHardened

-- | When Revealed: Place 1 resource on each Raider enemy in play. Each engaged non-Ship
-- enemy makes an immediate attack. If there are no engaged non-Ship enemies,
-- Battle-Hardened gains surge.
-- Shadow: If the attacking enemy is a Raider, place 2 resources on it.
instance RunMessage BattleHardened where
  runMessage msg t@(BattleHardened attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      raiders <- select (EnemyWithTrait Raider)
      for_ raiders \r -> placeResources attrs r 1
      players <- getRemainingPlayers
      engaged <- concatForM players \p -> map (p,) <$> select (EnemyEngagedWith (PlayerWithId p) <> not_ shipEnemy)
      for_ engaged \(p, e) -> immediateAttack p e
      when (null engaged) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) $ placeResources attrs eid 2
      pure t
    _ -> BattleHardened <$> runMessage msg attrs
