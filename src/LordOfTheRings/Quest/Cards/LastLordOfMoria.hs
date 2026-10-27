module LordOfTheRings.Quest.Cards.LastLordOfMoria (lastLordOfMoria) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers

newtype LastLordOfMoria = LastLordOfMoria QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lastLordOfMoria :: QuestCard LastLordOfMoria
lastLordOfMoria = quest LastLordOfMoria Cards.lastLordOfMoria

-- | When Revealed: Durin's Bane makes an immediate attack on the first player. Add Dark Pit
-- to the staging area.
-- Players cannot win the game while Durin's Bane is in play. If Durin's Bane leaves play by
-- the effect on Dark Pit, the players have won the game (see Dark Pit).
instance RunMessage LastLordOfMoria where
  runMessage msg q@(LastLordOfMoria attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      LastLordOfMoria <$> runBlockedStage durinsBane msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      traverse_ (immediateAttack lead) =<< getDurinsBane
      addSetAsideToStaging (cardIs Locations.darkPit)
      pure q
    _ -> LastLordOfMoria <$> runBlockedStage durinsBane msg attrs
