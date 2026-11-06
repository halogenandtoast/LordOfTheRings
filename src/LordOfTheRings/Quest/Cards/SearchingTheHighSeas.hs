module LordOfTheRings.Quest.Cards.SearchingTheHighSeas (searchingTheHighSeas) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Epic.Types (
  EpicReplica (epicShared),
  SharedKey (SearchingTheHighSeasProgress),
  stageOneComplete,
 )
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught.Helpers
import LordOfTheRings.Scenario.Types (scenarioEpic)

newtype SearchingTheHighSeas = SearchingTheHighSeas QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingTheHighSeas :: QuestCard SearchingTheHighSeas
searchingTheHighSeas = quest SearchingTheHighSeas Cards.searchingTheHighSeas

instance HasModifiersFor SearchingTheHighSeas where
  getModifiersFor (SearchingTheHighSeas a) = do
    players <- initialGroupSize
    total <- globalPlayerCount
    modifySelf a [ProgressPerRoundLimit $ 5 * players, QuestPointModifier $ 8 * total]

instance HasAbilities SearchingTheHighSeas where
  getAbilities (SearchingTheHighSeas a) =
    [mkAbility a 1 $ silent $ PhaseEndsWindow When QuestPhase | a.flipped]

instance RunMessage SearchingTheHighSeas where
  runMessage msg q@(SearchingTheHighSeas attrs) = case msg of
    DoStep 7 Setup -> askToPerformSetup attrs >> pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- globalPlayerCount
      replica <- getsGame (scenarioEpic . toAttrs . gameScenario)
      let complete = maybe (attrs.progress >= 8 * players) (stageOneComplete . epicShared) replica
      when complete $ push $ CompleteQuest attrs.id
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      -- Zero printed quest points keep the ordinary progress handler from advancing
      -- this stage before the end of the quest phase (stage 1B).
      changed <- runMessage msg attrs
      let added = changed.progress - attrs.progress
      when (added > 0) $ push $ ContributeShared SearchingTheHighSeasProgress added
      pure $ SearchingTheHighSeas changed
    RemoveTokens _ (isTarget attrs -> True) Progress _ -> do
      changed <- runMessage msg attrs
      let removed = attrs.progress - changed.progress
      when (removed > 0) $ push $ ContributeShared SearchingTheHighSeasProgress (negate removed)
      pure $ SearchingTheHighSeas changed
    -- Shared counters never replace local tokens: a local card effect can only
    -- remove progress placed by this group, not another group's contribution.
    _ -> SearchingTheHighSeas <$> runMessage msg attrs
