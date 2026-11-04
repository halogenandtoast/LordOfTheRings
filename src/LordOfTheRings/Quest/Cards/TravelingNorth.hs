module LordOfTheRings.Quest.Cards.TravelingNorth (travelingNorth) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyUpTheAnduin.Helpers (evilCreatureInPlay)

newtype TravelingNorth = TravelingNorth QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

travelingNorth :: QuestCard TravelingNorth
travelingNorth = quest TravelingNorth Cards.travelingNorth

-- | This stage cannot be defeated while an enemy from the Evil Creatures deck is in play.
instance HasModifiersFor TravelingNorth where
  getModifiersFor (TravelingNorth a) = when a.flipped do
    blocked <- evilCreatureInPlay
    when blocked $ modifySelf a [CannotBeDefeated]

-- | Forced: After placing progress here from questing successfully, place 1 resource here.
-- Forced: At the beginning of the encounter phase, remove 1 resource from this stage.
-- Then, add the top card of the Evil Creatures deck to the staging area.
instance HasAbilities TravelingNorth where
  getAbilities (TravelingNorth a) =
    [mkAbility a 1 $ forced $ PlacedProgressFromQuestingWindow After | a.flipped]
      <> [mkAbility a 2 $ forced $ PhaseBeginsWindow After EncounterPhase | a.flipped, countTokens Resource a.questTokens > 0]

{- | Setup: Set Woodmen Village and The Old Ford aside, out of play. Create the Evil
Creatures deck and place it next to the quest deck. Shuffle the encounter deck and discard
cards from the top until X locations are discarded, where X is the number of players. Add
each discarded location to the staging area.

The scenario set the two locations aside and made the Evil Creatures deck as it gathered
the encounter deck. With no resource here, none is removed and so no card is added: the
"then" needs it (FAQ 1.15).
-}
instance RunMessage TravelingNorth where
  runMessage msg q@(TravelingNorth attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType LocationType)
      TravelingNorth <$> runMessage msg attrs
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType LocationType) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      removeTokens attrs attrs Resource 1
      addFromEvilCreaturesDeck attrs
      pure q
    _ -> TravelingNorth <$> runMessage msg attrs
