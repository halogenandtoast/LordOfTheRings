module LordOfTheRings.Location.Cards.DwarvenWatchtower (dwarvenWatchtower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (gateInStaging, getGoblinsAssault, getHeroesDefense, goblinsAssaultProgress)

newtype DwarvenWatchtower = DwarvenWatchtower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

dwarvenWatchtower :: LocationCard DwarvenWatchtower
dwarvenWatchtower = location DwarvenWatchtower Cards.dwarvenWatchtower

-- | While Dwarven Watchtower is the active location, each hero gets +1 defense.
instance HasModifiersFor DwarvenWatchtower where
  getModifiersFor (DwarvenWatchtower a) = when (a.placement == ActiveLocation) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [DefenseModifier 1]

-- | Response: After Dwarven Watchtower is explored, the first player chooses: either place 1
-- resource on The Heroes' Defense, or remove 1 progress from The Goblins' Assault.
instance HasAbilities DwarvenWatchtower where
  getAbilities (DwarvenWatchtower a) = [mkAbility a 1 $ response $ ExploredWindow After a.id]

-- | Each option is offered only if it can be done in full (FAQ 1.44): no progress to remove,
-- or Gate of Gundabad in the staging area barring the resource, leaves the other.
instance RunMessage DwarvenWatchtower where
  runMessage msg l@(DwarvenWatchtower a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      defense <- getHeroesDefense
      assault <- getGoblinsAssault
      progress <- goblinsAssaultProgress
      gate <- selectAny gateInStaging
      chooseOrRunOneM lead do
        questionLabeled "dwarvenWatchtower.choose"
        causedBy a
        unless gate $ for_ defense \oid -> labeled "dwarvenWatchtower.resource" $ placeResources a (ObjectiveTarget oid) 1
        when (progress > 0) $ for_ assault \oid -> labeled "dwarvenWatchtower.progress" $ removeTokens a (ObjectiveTarget oid) Progress 1
      pure l
    _ -> DwarvenWatchtower <$> runMessage msg a
