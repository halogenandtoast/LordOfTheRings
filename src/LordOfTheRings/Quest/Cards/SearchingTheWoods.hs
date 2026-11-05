module LordOfTheRings.Quest.Cards.SearchingTheWoods (searchingTheWoods) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers

newtype SearchingTheWoods = SearchingTheWoods QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingTheWoods :: QuestCard SearchingTheWoods
searchingTheWoods = quest SearchingTheWoods Cards.searchingTheWoods

-- | If Frodo's Choice is not in play, any time players would place progress on this quest,
-- discard an equal number of cards from the encounter deck instead.
instance HasModifiersFor SearchingTheWoods where
  getModifiersFor (SearchingTheWoods a) = stageThreeModifiers a

-- | Response: After you quest successfully, discard a non-unique location in any staging
-- area.
instance HasAbilities SearchingTheWoods where
  getAbilities (SearchingTheWoods a) = [mkAbility a 1 $ response $ QuestedSuccessfullyWindow After | a.flipped]

{- | Forced: After Frodo's Choice is revealed at this stage, advance to stage 4A. (Side A's
Forced effect, after stage 2B is discarded, is the scenario's to resolve as it discards
stage 2B: see 'LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship'.) A location in
another staging area comes to this one to be discarded.
-}
instance RunMessage SearchingTheWoods where
  runMessage msg q@(SearchingTheWoods attrs) = case msg of
    ProgressCanceled (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeck attrs n
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      inPlay <- frodosChoiceInPlay
      if inPlay then SearchingTheWoods <$> runMessage msg attrs else q <$ discardTopOfEncounterDeck attrs n
    CheckWindows ws | attrs.flipped -> do
      revealed <- frodosChoiceRevealed ws
      when revealed advanceQuest
      SearchingTheWoods <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      here <- getStageLead
      locations <- nonUniqueLocationsInAnyStagingArea
      chooseOneM pid do
        questionLabeled "searchingTheWoods.discardLocation"
        causedBy attrs
        targets locations \l -> do
          placeInStagingAreaOf here l
          discard attrs l
      pure q
    _ -> SearchingTheWoods <$> runMessage msg attrs
