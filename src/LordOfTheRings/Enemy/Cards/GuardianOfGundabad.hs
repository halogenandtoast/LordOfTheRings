module LordOfTheRings.Enemy.Cards.GuardianOfGundabad (guardianOfGundabad) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype GuardianOfGundabad = GuardianOfGundabad EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardianOfGundabad :: EnemyCard GuardianOfGundabad
guardianOfGundabad = enemy GuardianOfGundabad Cards.guardianOfGundabad

-- | While there are 3 or more quest cards in the victory display, Guardian of Gundabad gets
-- +2 defense.
instance HasModifiersFor GuardianOfGundabad where
  getModifiersFor (GuardianOfGundabad a) = do
    quests <- questCardsInVictoryDisplay
    when (quests >= 3) $ modifySelf a [DefenseModifier 2]

-- | Shadow: Attacking enemy gets +2 defense until the end of the phase.
instance RunMessage GuardianOfGundabad where
  runMessage msg e@(GuardianOfGundabad attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 2)
      pure e
    _ -> GuardianOfGundabad <$> runMessage msg attrs
