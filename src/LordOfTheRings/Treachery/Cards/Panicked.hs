module LordOfTheRings.Treachery.Cards.Panicked (panicked) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, objectiveOf, rescuedVillagerCount)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Panicked = Panicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

panicked :: TreacheryCard Panicked
panicked = treachery Panicked Cards.panicked

-- | When Revealed: Each player must take a villager token from Rescued Villagers and place
-- it on a location in the staging area, if able. If no villager tokens were placed on a
-- location by this effect, Panicked! gains Surge.
-- Shadow: Attacking enemy gets +1 attack for each damage token on Dead Villagers.
instance RunMessage Panicked where
  runMessage msg t@(Panicked attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      rescued <- objectiveOf Objectives.rescuedVillagers
      available <- rescuedVillagerCount
      locations <- select LocationInStagingArea
      players <- getRemainingPlayers
      -- the first players in turn order take the villagers there are
      let taking = if null locations then [] else take available players
      for_ rescued \o -> for_ taking \p ->
        chooseOrRunOneM p do
          questionLabeled "panicked.placeVillager"
          causedBy attrs
          targets locations \l -> do
            removeTokens attrs (ObjectiveTarget o) Villager 1
            placeTokens attrs l Villager 1
      when (null taking) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid =<< deadVillagerCount
      pure t
    _ -> Panicked <$> runMessage msg attrs
