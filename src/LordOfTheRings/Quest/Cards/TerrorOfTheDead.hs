module LordOfTheRings.Quest.Cards.TerrorOfTheDead (terrorOfTheDead) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TerrorOfTheDead = TerrorOfTheDead QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

terrorOfTheDead :: QuestCard TerrorOfTheDead
terrorOfTheDead = quest TerrorOfTheDead Cards.terrorOfTheDead

instance HasAbilities TerrorOfTheDead where
  getAbilities (TerrorOfTheDead a) = [mkAbility a 1 $ forced RoundBeginsWindow | a.flipped]

instance RunMessage TerrorOfTheDead where
  runMessage msg q@(TerrorOfTheDead a) = case msg of
    Flip (isTarget a -> True) | not a.flipped -> do
      askToResolveWhenRevealed a
      TerrorOfTheDead <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      addSetAsideToStaging (cardIs Locations.theStoneOfErechStandalone)
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      stones <- select $ LocationWithCardCode (toCardCode Locations.theStoneOfErechStandalone)
      captive <- getSetAside (cardIs Objectives.derufin)
      for_ (headMay stones) \stone -> for_ (headMay captive) \card ->
        putObjectiveIntoPlayAttached card (LocationTarget stone)
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "erech.terrorChoice"
        causedBy a
        labeled "erech.skipPlanning" $ createEffect a GameTarget [SkipPhase PlanningPhase] UntilSpent
        labeled "erech.noResources" $ roundModifier a GameTarget CannotCollectResources
      pure q
    _ -> TerrorOfTheDead <$> runMessage msg a
