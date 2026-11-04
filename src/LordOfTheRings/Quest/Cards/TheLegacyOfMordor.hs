module LordOfTheRings.Quest.Cards.TheLegacyOfMordor (theLegacyOfMordor) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers

newtype TheLegacyOfMordor = TheLegacyOfMordor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLegacyOfMordor :: QuestCard TheLegacyOfMordor
theLegacyOfMordor = quest TheLegacyOfMordor Cards.theLegacyOfMordor

instance HasModifiersFor TheLegacyOfMordor where
  getModifiersFor (TheLegacyOfMordor a) = stage2Modifiers a

instance HasAbilities TheLegacyOfMordor where
  getAbilities (TheLegacyOfMordor a) = stage2Forced a

{- | Forced: At the beginning of the quest phase, each player deals 1 damage to a hero he
controls and exhausts that hero. That hero cannot ready or be healed until the refresh
phase: through the end of the combat phase, the last before it.
-}
instance RunMessage TheLegacyOfMordor where
  runMessage msg q@(TheLegacyOfMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "theLegacyOfMordor.hero"
          causedBy attrs
          targets heroes \h -> do
            dealDamage attrs h 1
            exhaust h
            createEffect attrs h [CannotReady, CannotBeHealed] (UntilEndOfPhaseOf CombatPhase)
      pure q
    _ -> TheLegacyOfMordor <$> runStage2 msg attrs
