module LordOfTheRings.Quest.Cards.TheRoadToIsenmouthe (theRoadToIsenmouthe) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheRoadToIsenmouthe = TheRoadToIsenmouthe QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRoadToIsenmouthe :: QuestCard TheRoadToIsenmouthe
theRoadToIsenmouthe = quest TheRoadToIsenmouthe Cards.theRoadToIsenmouthe

{- | Dire. Forced: After placing progress here from questing successfully, each player makes
a Fortitude test (none is made when the active location took all of it).
At the end of the quest phase, if the number of resources here is equal to or greater than
the number of players, advance to stage 3A.
-}
instance HasAbilities TheRoadToIsenmouthe where
  getAbilities (TheRoadToIsenmouthe a) =
    [ ability
    | a.flipped
    , ability <- [mkAbility a 1 $ silent $ PlacedProgressFromQuestingWindow After, mkAbility a 2 $ silent $ PhaseEndsWindow When QuestPhase]
    ]

{- | When Revealed: Search the encounter deck, discard pile, and victory display for Mordor
Road and add it to the staging area. Shuffle the encounter discard pile into the encounter
deck and travel to Mordor Road: any other active location returns to the staging area.

A player who passes the Fortitude test places a resource here. One who fails discards cards
from the top of the encounter deck until an enemy is discarded and puts that enemy into
play engaged with him: the deck read as the test resolves.
-}
instance RunMessage TheRoadToIsenmouthe where
  runMessage msg q@(TheRoadToIsenmouthe attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheRoadToIsenmouthe <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchEncounterCardsAndVictoryDisplay (cardIs Locations.mordorRoad) addToStaging
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      roads <- select (LocationWithCardCode (toCardCode Locations.mordorRoad) <> LocationInStagingArea)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theRoadToIsenmouthe.travel"
        causedBy attrs
        targets roads \road -> do
          active <- select IsActiveLocation
          for_ active (`placeLocation` StagingArea)
          travelWithoutCost road
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placed <- progressPlacedThisPhase (toTarget attrs)
      when (placed > 0) $ eachPlayer \pid ->
        fortitudeTest pid attrs (doStep 2 (WithTarget (PlayerTarget pid) msg)) (placeResources attrs attrs 1)
      pure q
    DoStep 2 (WithTarget (PlayerTarget pid) (UseCardAbility _ (isSource attrs -> True) 1 _)) -> do
      discardEncounterCardsUntilFor attrs (PlayerTarget pid) (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches $ CardWithType EnemyType) cards) (putEnemyIntoPlayEngagedFromDiscard pid)
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      players <- length <$> getRemainingPlayers
      when (countTokens Resource attrs.questTokens >= players) $ completeQuest attrs.id
      pure q
    _ -> TheRoadToIsenmouthe <$> runMessage msg attrs
