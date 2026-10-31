module LordOfTheRings.Enemy.Cards.SwarmingGoblins (swarmingGoblins) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype SwarmingGoblins = SwarmingGoblins EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swarmingGoblins :: EnemyCard SwarmingGoblins
swarmingGoblins = enemy SwarmingGoblins Cards.swarmingGoblins

-- | While you are engaged with Swarming Goblins, it gains: "Forced: When a quest card is
-- bypassed, search the encounter discard pile for a copy of Goblin Spearman and put it
-- into play engaged with you."
instance HasAbilities SwarmingGoblins where
  getAbilities (SwarmingGoblins a) =
    [restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) $ forced $ QuestBypassedWindow When]

instance RunMessage SwarmingGoblins where
  runMessage msg e@(SwarmingGoblins attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discards <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (placementController attrs.placement) \pid ->
        for_ (find ((== toCardCode Cards.goblinSpearman) . toCardCode) discards) \spearman -> do
          removeFromEncounterDiscard spearman
          putEnemyIntoPlayEngaged pid spearman
      pure e
    _ -> SwarmingGoblins <$> runMessage msg attrs
