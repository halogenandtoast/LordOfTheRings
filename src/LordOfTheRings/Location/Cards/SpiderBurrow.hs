module LordOfTheRings.Location.Cards.SpiderBurrow (spiderBurrow) where

import LordOfTheRings.Card (EncounterCard (..), toCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (flipSearchTo)

newtype SpiderBurrow = SpiderBurrow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

spiderBurrow :: LocationCard SpiderBurrow
spiderBurrow = location SpiderBurrow Cards.spiderBurrow

-- | Immune to player card effects.
instance HasModifiersFor SpiderBurrow where
  getModifiersFor (SpiderBurrow a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Spider Burrow becomes the active location, flip the Search objective to
-- On Track and add Brood Mother to the staging area.
-- Forced: When Spider Burrow leaves play as an explored location, set Brood Mother aside,
-- out of play (unless the players are at stage 3B).
instance HasAbilities SpiderBurrow where
  getAbilities (SpiderBurrow a) =
    [ mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)
    , mkAbility a 2 $ forced $ ExploredWindow When a.id
    ]

instance RunMessage SpiderBurrow where
  runMessage msg l@(SpiderBurrow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      flipSearchTo Objectives.onTrack
      addSetAsideToStaging (cardIs Enemies.broodMother)
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      atHive <- getsGame (any ((== toCardCode Quests.theSpidersHive) . toCardCode) . toList . (.entities.quests))
      mothers <- getsGame (filter ((== toCardCode Enemies.broodMother) . toCardCode) . toList . (.entities.enemies))
      unless atHive $ for_ mothers \m -> do
        removeFromGame (toId m)
        setAside $ MkEncounterCard (toCard m).id (toCardCode m)
      pure l
    _ -> SpiderBurrow <$> runMessage msg attrs
