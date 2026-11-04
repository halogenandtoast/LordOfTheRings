module LordOfTheRings.Treachery.Cards.HatredRekindledTheBattleOfFiveArmies (hatredRekindledTheBattleOfFiveArmies) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HatredRekindled = HatredRekindled TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Hatred Rekindled of The Battle of Five Armies.
hatredRekindledTheBattleOfFiveArmies :: TreacheryCard HatredRekindled
hatredRekindledTheBattleOfFiveArmies = treachery HatredRekindled Cards.hatredRekindledTheBattleOfFiveArmies

{- | When Revealed: Each enemy engaged with a player makes an immediate attack. Do not deal
shadow cards for attacks made by this effect. If no attacks were made by this effect, this
card gains surge. The attacks are made player by player, in turn order.
Shadow: Attacking enemy makes an additional attack immediately after this one.
-}
instance RunMessage HatredRekindled where
  runMessage msg t@(HatredRekindled attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      order <- getPlayerOrder
      attacks <- concat <$> for order \pid -> map (pid,) <$> select (EnemyEngagedWith $ PlayerWithId pid)
      for_ attacks \(pid, eid) -> immediateAttackWithoutShadow pid eid
      when (null attacks) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> HatredRekindled <$> runMessage msg attrs
