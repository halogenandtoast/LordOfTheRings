module LordOfTheRings.Quest.Cards.RacingNorth (racingNorth) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype RacingNorth = RacingNorth QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

racingNorth :: QuestCard RacingNorth
racingNorth = quest RacingNorth Cards.racingNorth

-- | Uruk Chieftain cannot take damage.
instance HasModifiersFor RacingNorth where
  getModifiersFor (RacingNorth a) = when a.flipped do
    getUrukChieftains >>= traverse_ \e -> modified a e [CannotTakeDamage]

-- | Forced: After characters are committed to quest, no more than 5 progress can be placed
-- here this round (10 progress instead if each Tamed Mumak in play is committed to the
-- quest). See 'limitProgressThisRound'.
instance HasAbilities RacingNorth where
  getAbilities (RacingNorth a) = [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | When Revealed: Choose the staging area or the Orcs' area. Add the set aside Uruk
-- Warg-rider to the chosen area.
instance RunMessage RacingNorth where
  runMessage msg q@(RacingNorth attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      RacingNorth <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      riders <- getsGame (filter (cardMatches $ cardIs Enemies.urukWargRider) . (.scenarioSetAside) . toAttrs . (.scenario))
      lead <- getLead
      for_ (take 1 riders) \rider -> chooseOneM lead do
        questionLabeled "racingNorth.choose"
        causedBy attrs
        labeled "racingNorth.staging" $ addToStaging rider
        labeled "racingNorth.orcsArea" $ addCardToOrcsArea rider
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      limitProgressThisRound attrs
      pure q
    _ -> RacingNorth <$> runMessage msg attrs
