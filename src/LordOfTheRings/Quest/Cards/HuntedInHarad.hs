module LordOfTheRings.Quest.Cards.HuntedInHarad (huntedInHarad) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers

newtype HuntedInHarad = HuntedInHarad QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

huntedInHarad :: QuestCard HuntedInHarad
huntedInHarad = quest HuntedInHarad Cards.huntedInHarad

-- | Uruk Chieftain cannot take damage. Reveal an additional encounter card during the
-- quest phase if Uruk Chieftain is at stage 2D: the Orcs' stage is 2D.
instance HasModifiersFor HuntedInHarad where
  getModifiersFor (HuntedInHarad a) = when a.flipped do
    getUrukChieftains >>= traverse_ \e -> modified a e [CannotTakeDamage]
    orcs <- getOrcsStage
    when (orcs == 2) $ modified a GameTarget [AdditionalStagingReveals 1]

-- | Forced: After characters are committed to quest, no more than 5 progress can be placed
-- here this round (10 progress instead if each Tamed Mumak in play is committed to the
-- quest). See 'limitProgressThisRound'.
instance HasAbilities HuntedInHarad where
  getAbilities (HuntedInHarad a) = [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
-- cards from the top until an Orc enemy is discarded. Add that enemy to the Orcs' area.
instance RunMessage HuntedInHarad where
  runMessage msg q@(HuntedInHarad attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      HuntedInHarad <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardEncounterCardsUntil attrs orcEnemy
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      addOrcToOrcsArea cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      limitProgressThisRound attrs
      pure q
    _ -> HuntedInHarad <$> runMessage msg attrs
