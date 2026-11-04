module LordOfTheRings.Location.Cards.Ravenhill (ravenhill) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (removeProgressFromStage, stageTravelCost)
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype Ravenhill = Ravenhill LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenhill :: LocationCard Ravenhill
ravenhill = location Ravenhill Cards.ravenhill

-- | Immune to player card effects.
-- Travel: Spend 3 Baggins resources to travel here. (1 Baggins resource instead if stage
-- 4B is the current quest.) The players' as a group.
instance HasModifiersFor Ravenhill where
  getModifiersFor (Ravenhill a) = do
    cost <- stageTravelCost Quests.theDefenseOfRavenhill
    baggins <- getGroupResourcesOfSphere Baggins
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | baggins < cost]

-- | Forced: Remove 2 progress from stage 4B at the end of each round, if able.
instance HasAbilities Ravenhill where
  getAbilities (Ravenhill a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage Ravenhill where
  runMessage msg l@(Ravenhill a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromStage a Quests.theDefenseOfRavenhill 2
      pure l
    TravelTo lid | lid == a.id -> do
      cost <- stageTravelCost Quests.theDefenseOfRavenhill
      lead <- getLead
      spendGroupResources lead Baggins cost
      Ravenhill <$> runMessage msg a
    _ -> Ravenhill <$> runMessage msg a
