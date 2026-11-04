module LordOfTheRings.Quest.Cards.DontLetThemEscape (dontLetThemEscape) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DontLetThemEscape = DontLetThemEscape QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (The Black Serpent).
dontLetThemEscape :: QuestCard DontLetThemEscape
dontLetThemEscape = quest DontLetThemEscape Cards.dontLetThemEscape

-- | Forced: At the end of the round, discard all progress from the main quest.
instance HasAbilities DontLetThemEscape where
  getAbilities (DontLetThemEscape a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Forced: After this quest is chosen as the current quest, reveal an encounter card.
instance RunMessage DontLetThemEscape where
  runMessage msg q@(DontLetThemEscape attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeProgressFromMainQuest attrs =<< getMainQuestProgress
      pure q
    SetCurrentQuest (isTarget attrs -> True) -> do
      revealEncounterCard
      DontLetThemEscape <$> runMessage msg attrs
    _ -> DontLetThemEscape <$> runMessage msg attrs
