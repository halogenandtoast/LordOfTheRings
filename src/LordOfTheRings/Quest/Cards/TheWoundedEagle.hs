module LordOfTheRings.Quest.Cards.TheWoundedEagle (theWoundedEagle) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype TheWoundedEagle = TheWoundedEagle QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWoundedEagle :: QuestCard TheWoundedEagle
theWoundedEagle = quest TheWoundedEagle Cards.theWoundedEagle

-- | Setup: Search the encounter deck for Rhosgobel and Wilyador, and add them to the
-- staging area with 2 damage tokens on Wilyador. Then, shuffle the encounter deck.
instance RunMessage TheWoundedEagle where
  runMessage msg q@(TheWoundedEagle attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
      searchEncounterDeckToStaging lead (cardIs Locations.rhosgobel)
      searchEncounterDeckToStaging lead (cardIs Objectives.wilyador)
      -- Wilyador joins the first player as he comes out, keeping his card
      for_ [c | c <- deck, toCardCode c == toCardCode Objectives.wilyador] \c ->
        placeTokens attrs c Damage 2
      pure $ TheWoundedEagle $ attrs {questFlipped = True}
    _ -> TheWoundedEagle <$> runMessage msg attrs
