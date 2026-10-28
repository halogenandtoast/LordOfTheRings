module LordOfTheRings.Quest.Cards.AmbushInIthilien (ambushInIthilien) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AmbushInIthilien = AmbushInIthilien QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushInIthilien :: QuestCard AmbushInIthilien
ambushInIthilien = quest AmbushInIthilien Cards.ambushInIthilien

-- | Setup: Add Celador to the staging area. Search the encounter deck for a copy of
-- Ithilien Road and make it the active location. Each player must search the encounter
-- deck for a copy of Southron Company and add it to the staging area. Shuffle the
-- encounter deck.
-- Battle. If the players complete this stage with Celador in the staging area, advance to
-- stage 3A (bypassing stage 2): the scenario takes stage 2 out of the quest deck.
instance RunMessage AmbushInIthilien where
  runMessage msg q@(AmbushInIthilien attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter ((== toCardCode def) . toCardCode) deck
      for_ (take 1 $ copies Objectives.celador) \celador -> do
        removeFromEncounterDeck celador
        addToStaging celador
      for_ (take 1 $ copies Locations.ithilienRoad) \road -> do
        removeFromEncounterDeck road
        putIntoPlayAsActiveLocation road
      players <- length <$> getRemainingPlayers
      for_ (take players $ copies Enemies.southronCompany) \company -> do
        removeFromEncounterDeck company
        addToStaging company
      shuffleEncounterDeck
      pure $ AmbushInIthilien $ attrs {questFlipped = True}
    _ -> AmbushInIthilien <$> runMessage msg attrs
