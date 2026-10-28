module LordOfTheRings.Enemy.Cards.ForestBat (forestBat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyThreat)

newtype ForestBat = ForestBat EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestBat :: EnemyCard ForestBat
forestBat = enemy ForestBat Cards.forestBat

-- | When Revealed: The first player deals 2 damage to a questing hero and removes that
-- hero from the quest.
-- Shadow: Defending player raises his threat by X where X is the attacking enemy's threat.
instance RunMessage ForestBat where
  runMessage msg e@(ForestBat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      questing <- select CommittedHero
      chooseOrRunOneM lead do
        questionLabeled "forestBat.damage"
        causedBy attrs
        targets questing \h -> do
          dealDamage attrs h 2
          removeFromQuest h
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid =<< getEnemyThreat eid
      pure e
    _ -> ForestBat <$> runMessage msg attrs
