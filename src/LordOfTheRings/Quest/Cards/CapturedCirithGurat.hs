module LordOfTheRings.Quest.Cards.CapturedCirithGurat (capturedCirithGurat) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype CapturedCirithGurat = CapturedCirithGurat QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Dungeons of Cirith Gurat's encounter side quest "Captured!" (The Blood of Gondor
-- has a stage of that name).
capturedCirithGurat :: QuestCard CapturedCirithGurat
capturedCirithGurat = quest CapturedCirithGurat Cards.capturedCirithGurat

{- | When Revealed: Each player loses control of an ally he controls and attaches it to this
stage. If no allies are attached to this stage, discard it and reveal an additional
encounter card.
Forced: When this stage is defeated, each player reduces his threat by X, where X is the
cost of the ally he controls that was attached to this stage: read as it is defeated, its
captives still on it, before they are rescued.
-}
instance RunMessage CapturedCirithGurat where
  runMessage msg q@(CapturedCirithGurat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "capturedCirithGurat.ally"
          causedBy attrs
          targets allies \al -> guardCharacterWith al attrs
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      captives <- charactersGuardedBy attrs.cardId
      when (null captives) do
        discard attrs
        revealEncounterCard
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      captives <- charactersGuardedBy attrs.cardId
      for_ [aid | AllyTarget aid <- captives] \aid -> do
        a <- getAlly aid
        lowerThreatBy attrs a.owner (printedCostOf a)
      CapturedCirithGurat <$> runMessage msg attrs
    _ -> CapturedCirithGurat <$> runMessage msg attrs
