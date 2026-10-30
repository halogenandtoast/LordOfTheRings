module LordOfTheRings.Enemy.Cards.ServantOfTheDeceiver (servantOfTheDeceiver) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ServantOfTheDeceiver = ServantOfTheDeceiver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

servantOfTheDeceiver :: EnemyCard ServantOfTheDeceiver
servantOfTheDeceiver = enemy ServantOfTheDeceiver Cards.servantOfTheDeceiver

-- | Forced: When Servant of the Deceiver engages you, look at the top card of your deck. If
-- it is printed cost 2 or less, place it on the bottom of your deck and Servant of the
-- Deceiver gets +2 attack until the end of the round.
instance HasAbilities ServantOfTheDeceiver where
  getAbilities (ServantOfTheDeceiver a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage ServantOfTheDeceiver where
  runMessage msg e@(ServantOfTheDeceiver attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        top <- headMay . (.deck) <$> getPlayer pid
        for_ top \c -> when (printedCostOf c <= 2) do
          moveToBottomOfDeck pid c
          roundModifier attrs attrs (AttackModifier 2)
      pure e
    _ -> ServantOfTheDeceiver <$> runMessage msg attrs
