module LordOfTheRings.Location.Cards.TheFrontGate (theFrontGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (removeProgressFromStage, stageTravelCost)
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype TheFrontGate = TheFrontGate LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theFrontGate :: LocationCard TheFrontGate
theFrontGate = location TheFrontGate Cards.theFrontGate

-- | Immune to player card effects.
-- Travel: Spend 3 Baggins resources to travel here. (1 Baggins resource instead if stage
-- 2B is the current quest.) The players' as a group.
instance HasModifiersFor TheFrontGate where
  getModifiersFor (TheFrontGate a) = do
    cost <- stageTravelCost Quests.theEasternSpur
    baggins <- getGroupResourcesOfSphere Baggins
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | baggins < cost]

-- | Forced: Remove 2 progress from stage 2B at the end of each round, if able.
instance HasAbilities TheFrontGate where
  getAbilities (TheFrontGate a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage TheFrontGate where
  runMessage msg l@(TheFrontGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromStage a Quests.theEasternSpur 2
      pure l
    TravelTo lid | lid == a.id -> do
      cost <- stageTravelCost Quests.theEasternSpur
      lead <- getLead
      spendGroupResources lead Baggins cost
      TheFrontGate <$> runMessage msg a
    _ -> TheFrontGate <$> runMessage msg a
