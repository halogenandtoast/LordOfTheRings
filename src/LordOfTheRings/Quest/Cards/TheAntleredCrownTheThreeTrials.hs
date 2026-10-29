module LordOfTheRings.Quest.Cards.TheAntleredCrownTheThreeTrials (theAntleredCrownTheThreeTrials) where

import LordOfTheRings.Card (Card (EncounterCard), HasCardDef (..), cdCardTraits, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers
import LordOfTheRings.Trait (Trait (Guardian))

newtype TheAntleredCrownTheThreeTrials = TheAntleredCrownTheThreeTrials QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theAntleredCrownTheThreeTrials :: QuestCard TheAntleredCrownTheThreeTrials
theAntleredCrownTheThreeTrials = quest TheAntleredCrownTheThreeTrials Cards.theAntleredCrownTheThreeTrials

-- | When Revealed: Add Hallowed Circle to the staging area. Reveal each Guardian enemy in
-- the victory display, adding them to the staging area.
-- Each Guardian enemy engages the player who controls the Key objective with a Trait
-- shared by that enemy (the scenario sees to it).
-- Players cannot defeat this stage unless Hallowed Circle is in the victory display. If the
-- players defeat this stage, they win the game.
instance RunMessage TheAntleredCrownTheThreeTrials where
  runMessage msg q@(TheAntleredCrownTheThreeTrials attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheAntleredCrownTheThreeTrials <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.hallowedCircle)
      victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      for_ [c | EncounterCard c <- victory, Guardian `member` cdCardTraits (toCardDef c)] revealFromVictoryDisplay
      pure q
    _ -> TheAntleredCrownTheThreeTrials <$> runConditionalStage circleWon msg attrs
   where
    circleWon =
      getsGame (any ((== toCardCode Locations.hallowedCircle) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario))
