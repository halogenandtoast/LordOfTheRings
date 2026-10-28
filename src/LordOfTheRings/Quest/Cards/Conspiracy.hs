module LordOfTheRings.Quest.Cards.Conspiracy (conspiracy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (investigation, runInvestigation)

newtype Conspiracy = Conspiracy QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

conspiracy :: QuestCard Conspiracy
conspiracy = quest Conspiracy Cards.conspiracy

-- | Forced: After the active location leaves play as an explored location, place 1
-- resource token on this quest. If there are 4 or more resource tokens on Conspiracy,
-- advance to the next stage.
instance HasAbilities Conspiracy where
  getAbilities (Conspiracy a) = [investigation a]

-- | Setup: the scenario builds the underworld deck and sets the Villain, the Plot and
-- Roots of Mindolluin aside as it gathers the encounter deck.
-- When Revealed: Search the encounter deck for The Fourth Star and make it the active
-- location. Shuffle the encounter deck.
instance RunMessage Conspiracy where
  runMessage msg q@(Conspiracy attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      askToResolveWhenRevealed attrs
      pure $ Conspiracy $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find ((== toCardCode Locations.theFourthStar) . toCardCode) deck) \star -> do
        removeFromEncounterDeck star
        putIntoPlayAsActiveLocation star
      shuffleEncounterDeck
      pure q
    _ -> Conspiracy <$> runInvestigation msg attrs
