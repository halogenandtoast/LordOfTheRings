module LordOfTheRings.Quest.Cards.FleeingFromTharbad (fleeingFromTharbad) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FleeingFromTharbad = FleeingFromTharbad QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fleeingFromTharbad :: QuestCard FleeingFromTharbad
fleeingFromTharbad = quest FleeingFromTharbad Cards.fleeingFromTharbad

-- | Setup: The first player takes control of Nalir. Set the Ancient Marsh-dweller aside,
-- out of play. Each player searches the encounter deck for a different location and adds
-- it to the staging area. Shuffle the encounter deck.
-- When Revealed: Advance to a random stage 2A.
--
-- Each player searches in turn, from the first player, for a location unlike those already
-- in the staging area; the stage then leaves for one of the stage 2s, as the scenario picks.
instance RunMessage FleeingFromTharbad where
  runMessage msg q@(FleeingFromTharbad attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Objectives.nalirTheNinInEilph)
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ setAside $ filter (cardMatches $ cardIs Enemies.ancientMarshDweller) deck
      doStep 1 msg
      pure $ FleeingFromTharbad $ attrs {questFlipped = True}
    DoStep n (Flip (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (Flip $ toTarget attrs)
        [] -> do
          shuffleEncounterDeck
          completeQuest attrs.id
      pure q
    _ -> FleeingFromTharbad <$> runMessage msg attrs
