module LordOfTheRings.Enemy.Cards.TraitorousWight (traitorousWight) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (removeProgressPerDamage)

newtype TraitorousWight = TraitorousWight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

traitorousWight :: EnemyCard TraitorousWight
traitorousWight = enemy TraitorousWight Cards.traitorousWight

-- | Forced: When Traitorous Wight engages a player, remove 2 progress from each quest card
-- in play.
instance HasAbilities TraitorousWight where
  getAbilities (TraitorousWight a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

-- | Shadow: For each point of damage dealt by this attack, remove 1 progress from each
-- quest in play.
instance RunMessage TraitorousWight where
  runMessage msg e@(TraitorousWight attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeProgressFromEachQuest attrs 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      removeProgressPerDamage (CardIdSource cid) eid
      pure e
    _ -> TraitorousWight <$> runMessage msg attrs
