module LordOfTheRings.Quest.Cards.ContinuingEastward (continuingEastward) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (locateTest, triggerLostEffects)

newtype ContinuingEastward = ContinuingEastward QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

continuingEastward :: QuestCard ContinuingEastward
continuingEastward = quest ContinuingEastward Cards.continuingEastward

-- | If the players quest unsuccessfully, trigger all "Lost:" effects in play.
instance HasAbilities ContinuingEastward where
  getAbilities (ContinuingEastward a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After]

-- | When Revealed: The first player makes a locate test. If this test is failed, reveal
-- cards from the encounter deck equal to the number of players in the game and add them to
-- the staging area. Then, trigger all "Lost:" effects in play.
-- If the players defeat this stage, they have won the game.
instance RunMessage ContinuingEastward where
  runMessage msg q@(ContinuingEastward attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      ContinuingEastward <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      locateTest attrs (revealEncounterCards players >> triggerLostEffects attrs) (pure ())
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      triggerLostEffects attrs
      pure q
    _ -> ContinuingEastward <$> runMessage msg attrs
