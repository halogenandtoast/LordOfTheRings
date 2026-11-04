module LordOfTheRings.Quest.Cards.MemoryOfTheDarkYears (memoryOfTheDarkYears) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers

newtype MemoryOfTheDarkYears = MemoryOfTheDarkYears QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

memoryOfTheDarkYears :: QuestCard MemoryOfTheDarkYears
memoryOfTheDarkYears = quest MemoryOfTheDarkYears Cards.memoryOfTheDarkYears

instance HasModifiersFor MemoryOfTheDarkYears where
  getModifiersFor (MemoryOfTheDarkYears a) = stage2Modifiers a

instance HasAbilities MemoryOfTheDarkYears where
  getAbilities (MemoryOfTheDarkYears a) = stage2Forced a

{- | Forced: At the beginning of the quest phase, discard X cards from the top of the
encounter deck, where X is 1 more than the number of quest stages in the victory display.
Add 2 to the total Threat in the staging area for each different card type discarded this
way: for this quest phase, the one it begins.
-}
instance RunMessage MemoryOfTheDarkYears where
  runMessage msg q@(MemoryOfTheDarkYears attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- templeX
      discardTopOfEncounterDeck attrs x
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let types = length $ nub $ map (cdCardType . toCardDef) cards
      when (types > 0) $ phaseModifier attrs GameTarget (StagingThreatBonus $ 2 * types)
      pure q
    _ -> MemoryOfTheDarkYears <$> runStage2 msg attrs
