module LordOfTheRings.Quest.Cards.SavageryOfTheOrcs (savageryOfTheOrcs) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (getVillagers, objectiveOf)

newtype SavageryOfTheOrcs = SavageryOfTheOrcs QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageryOfTheOrcs :: QuestCard SavageryOfTheOrcs
savageryOfTheOrcs = quest SavageryOfTheOrcs Cards.savageryOfTheOrcs

-- | Setup: Set Ghulat aside, out of play. Put Lord Alcaron into play. Make Burning
-- Farmhouse the active location. Add the Rescued Villagers and Dead Villagers objectives
-- to the staging area. Then, shuffle the encounter deck and reveal 1 encounter card per
-- player and add it to the staging area.
-- Villagers 5.
-- When progress would be placed on Savagery of the Orcs, move an equal number of villager
-- tokens from this quest onto Rescued Villagers instead. If there are no villager tokens
-- on Savagery of the Orcs, advance to the next stage.
instance RunMessage SavageryOfTheOrcs where
  runMessage msg q@(SavageryOfTheOrcs attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let find' def = find ((== toCardCode def) . toCardCode) deck
      traverse_ setAside (find' Enemies.ghulat)
      for_ (find' Objectives.lordAlcaron) \alcaron -> do
        removeFromEncounterDeck alcaron
        addToStaging alcaron
      for_ (find' Locations.burningFarmhouse) \farmhouse -> do
        removeFromEncounterDeck farmhouse
        putIntoPlayAsActiveLocation farmhouse
      for_ (mapMaybe find' [Objectives.rescuedVillagers, Objectives.deadVillagers]) \o -> do
        removeFromEncounterDeck o
        addToStaging o
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      placeTokens attrs attrs Villager 5
      pure $ SavageryOfTheOrcs $ attrs {questFlipped = True}
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      villagers <- getVillagers (toTarget attrs)
      let moved = min n villagers
      rescued <- objectiveOf Objectives.rescuedVillagers
      when (moved > 0) do
        removeTokens attrs attrs Villager moved
        for_ rescued \o -> placeTokens attrs (ObjectiveTarget o) Villager moved
      when (villagers - moved == 0) $ push (CompleteQuest attrs.id)
      pure q
    _ -> SavageryOfTheOrcs <$> runMessage msg attrs
