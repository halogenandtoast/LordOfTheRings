module LordOfTheRings.Quest.Cards.GornakhsDungeons (gornakhsDungeons) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameSeparation)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (captureFromDeck, capturedUnder, rescueAtRandom)

newtype GornakhsDungeons = GornakhsDungeons QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gornakhsDungeons :: QuestCard GornakhsDungeons
gornakhsDungeons = quest GornakhsDungeons Cards.gornakhsDungeons

-- | Capture 7.
-- When Revealed: Search the encounter deck for 1 copy of Prison Cell and add it to your
-- staging area. Then, capture an additional 2 cards from the top of your captured deck
-- underneath that copy of Prison Cell.
-- (2B) Any time players would place progress tokens on this quest, rescue an equal number
-- of captured cards at random from underneath it instead.
-- Forced: If there are no captured cards underneath Gornákh's Dungeons, join another
-- player at the beginning of the travel phase. If you cannot join another player, all
-- players advance to stage 3A together.
--
-- The Forced effect is checked as the area ends its quest phase, the areas joining at the
-- beginning of the travel phase; with no other area to join, the stage is defeated.
instance RunMessage GornakhsDungeons where
  runMessage msg q@(GornakhsDungeons attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      captureFromDeck attrs 7
      askToResolveWhenRevealed attrs
      GornakhsDungeons <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- the nightmare Prison Cell stands in for those the Nightmare Deck removes
      let cells = map toCardCode [Locations.prisonCell, Locations.prisonCellNightmare]
      for_ (take 1 $ filter ((`elem` cells) . toCardCode) deck) \cell -> do
        removeFromEncounterDeck cell
        addToStaging cell
        captureFromDeck (CardIdTarget cell.id) 2
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      rescueAtRandom attrs.cardId n
      pure q
    EndPhase QuestPhase -> do
      none <- null <$> capturedUnder attrs.cardId
      separated <- getsGame (isJust . gameSeparation)
      when none $ if separated then joinAnotherPlayer else completeQuest attrs.id
      pure q
    -- the scenario keeps the cards captured underneath it
    PlaceUnderneath (isTarget attrs -> True) _ -> pure q
    _ -> GornakhsDungeons <$> runMessage msg attrs
