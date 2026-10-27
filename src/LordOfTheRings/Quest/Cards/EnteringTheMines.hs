module LordOfTheRings.Quest.Cards.EnteringTheMines (enteringTheMines) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype EnteringTheMines = EnteringTheMines QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

enteringTheMines :: QuestCard EnteringTheMines
enteringTheMines = quest EnteringTheMines Cards.enteringTheMines

-- | Setup: Search the encounter deck for East-gate and Cave Torch. Put East-gate into play
-- as the active location, and have the first player attach Cave Torch to a hero of his
-- choice. Set First Hall and Bridge of Khazad-dûm aside, out of play. Shuffle the
-- encounter deck.
-- When Revealed: Reveal 1 encounter card per player, and add it to the staging area.
-- Players cannot advance to the next stage of the scenario unless Bridge of Khazad-dûm is
-- in their victory display.
instance RunMessage EnteringTheMines where
  runMessage msg q@(EnteringTheMines attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let find' def = find ((== toCardCode def) . toCardCode) deck
      for_ (find' Locations.eastGate) \gate -> do
        removeFromEncounterDeck gate
        putIntoPlayAsActiveLocation gate
      for_ (find' Objectives.caveTorch) \torch -> do
        removeFromEncounterDeck torch
        lead <- getLead
        heroes <- select AnyHero
        chooseOrRunOneM lead do
          questionLabeled "enteringTheMines.attach"
          causedBy attrs
          targets heroes (putObjectiveIntoPlayAttached torch)
      traverse_ setAside $ mapMaybe find' [Locations.firstHall, Locations.bridgeOfKhazadDum]
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      pure $ EnteringTheMines $ attrs {questFlipped = True}
    _ -> EnteringTheMines <$> runConditionalStage condition msg attrs
   where
    condition = getsGame (any (cardMatches $ cardIs Locations.bridgeOfKhazadDum) . (.victoryDisplay) . toAttrs . (.scenario))
