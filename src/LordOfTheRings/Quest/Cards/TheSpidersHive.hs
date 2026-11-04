module LordOfTheRings.Quest.Cards.TheSpidersHive (theSpidersHive) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (flipSearchTo)

newtype TheSpidersHive = TheSpidersHive QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersHive :: QuestCard TheSpidersHive
theSpidersHive = quest TheSpidersHive Cards.theSpidersHive

-- | This stage cannot be defeated while Brood Mother is in play. If the players defeat this
-- stage, they win the game (the last stage). Brood Mother herself reads "while there are
-- at least 5 progress tokens on this stage, Brood Mother loses the text: 'Cannot take
-- damage.'"
instance HasModifiersFor TheSpidersHive where
  getModifiersFor (TheSpidersHive a) = when a.flipped do
    whenM (selectAny $ EnemyWithCardCode $ toCardCode Enemies.broodMother) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the Search objective is flipped to Off Track, remove all progress tokens
-- from this stage.
instance HasAbilities TheSpidersHive where
  getAbilities (TheSpidersHive a) = [mkAbility a 1 $ forced $ FlippedToWindow After (toCardCode Objectives.offTrack) | a.flipped]

{- | When Revealed (3A): The first player adds Brood Mother to the staging area. Each other
player reveals an encounter card. Flip the Search objective to Off Track. Brood Mother,
already in play from Spider Burrow, stays where she is.
-}
instance RunMessage TheSpidersHive where
  runMessage msg q@(TheSpidersHive attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheSpidersHive <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.broodMother)
      players <- length <$> getRemainingPlayers
      revealEncounterCards (players - 1)
      flipSearchTo Objectives.offTrack
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      when (attrs.progress > 0) $ removeTokens attrs attrs Progress attrs.progress
      pure q
    _ -> TheSpidersHive <$> runMessage msg attrs
