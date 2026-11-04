module LordOfTheRings.Location.Cards.RuinsOfDale (ruinsOfDale) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (removeProgressFromStage, stageTravelCost)
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype RuinsOfDale = RuinsOfDale LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfDale :: LocationCard RuinsOfDale
ruinsOfDale = location RuinsOfDale Cards.ruinsOfDale

-- | Immune to player card effects.
-- Travel: Spend 3 Baggins resources to travel here. (1 Baggins resource instead if stage
-- 3B is the current quest.) The players' as a group.
instance HasModifiersFor RuinsOfDale where
  getModifiersFor (RuinsOfDale a) = do
    cost <- stageTravelCost Quests.battleInTheDale
    baggins <- getGroupResourcesOfSphere Baggins
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | baggins < cost]

-- | Forced: Remove 2 progress from stage 3B at the end of each round, if able.
instance HasAbilities RuinsOfDale where
  getAbilities (RuinsOfDale a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage RuinsOfDale where
  runMessage msg l@(RuinsOfDale a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromStage a Quests.battleInTheDale 2
      pure l
    TravelTo lid | lid == a.id -> do
      cost <- stageTravelCost Quests.battleInTheDale
      lead <- getLead
      spendGroupResources lead Baggins cost
      RuinsOfDale <$> runMessage msg a
    _ -> RuinsOfDale <$> runMessage msg a
