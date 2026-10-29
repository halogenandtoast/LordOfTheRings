module LordOfTheRings.Quest.Cards.SearchingForMugash (searchingForMugash) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (getMugash)

newtype SearchingForMugash = SearchingForMugash QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingForMugash :: QuestCard SearchingForMugash
searchingForMugash = quest SearchingForMugash Cards.searchingForMugash

-- | Time 2.
-- Forced: After the last time counter is removed for this stage, shuffle the discard pile
-- into the encounter deck and reveal 2 encounter cards per player. Place 2 time counters
-- on this stage.
-- Response: After questing successfully, cancel all progress that would be placed on this
-- stage. Then, place 1 time counter on this stage. Or, if Mugash is in play, advance to
-- stage 3.
-- ponytail: the progress is placed as the quest resolves and the window after it opens
-- only then, so the Response removes the progress on the stage rather than cancel it; the
-- stage has no quest points, so its progress does nothing meanwhile.
instance HasAbilities SearchingForMugash where
  getAbilities (SearchingForMugash a) =
    [ mkAbility a 1 $ forced $ lastTimeCounterRemoved a
    , mkAbility a 2 $ response $ QuestedSuccessfullyWindow After
    ]

instance RunMessage SearchingForMugash where
  runMessage msg q@(SearchingForMugash attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      players <- getRemainingPlayers
      revealEncounterCards (2 * length players)
      placeTimeCounters attrs attrs 2
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      when (attrs.progress > 0) $ removeTokens attrs attrs Progress attrs.progress
      mugash <- getMugash
      if isJust mugash then completeQuest attrs.id else placeTimeCounters attrs attrs 1
      pure q
    _ -> SearchingForMugash <$> runMessage msg attrs
