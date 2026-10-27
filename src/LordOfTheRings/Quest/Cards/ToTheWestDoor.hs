module LordOfTheRings.Quest.Cards.ToTheWestDoor (toTheWestDoor) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ToTheWestDoor = ToTheWestDoor QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

toTheWestDoor :: QuestCard ToTheWestDoor
toTheWestDoor = quest ToTheWestDoor Cards.toTheWestDoor

-- | Setup: Remove The Watcher and Doors of Durin from the encounter deck and set them
-- aside, out of play.
-- When Revealed: Reveal cards from the encounter deck and add them to the staging area
-- until there is at least X threat in the staging area. X is twice the number of players
-- in the game.
instance RunMessage ToTheWestDoor where
  runMessage msg q@(ToTheWestDoor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ setAside $ filter (\c -> any (`cardMatches` c) [cardIs Enemies.theWatcher, cardIs Locations.doorsOfDurin]) deck
      doStep 1 msg
      pure $ ToTheWestDoor $ attrs {questFlipped = True}
    -- one card at a time, each resolving before the threat is counted again
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      threat <- getStagingThreat
      players <- length <$> getRemainingPlayers
      scenario <- getsGame (toAttrs . (.scenario))
      when (threat < 2 * players && (notNull scenario.encounterDeck || notNull scenario.encounterDiscard)) do
        revealEncounterCard
        doStep 1 (Flip $ toTarget attrs)
      pure q
    _ -> ToTheWestDoor <$> runMessage msg attrs
