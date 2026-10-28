module LordOfTheRings.Objective.Cards.UnholyAlliance (unholyAlliance) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (attackDealingDamage)

newtype UnholyAlliance = UnholyAlliance ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

unholyAlliance :: ObjectiveCard UnholyAlliance
unholyAlliance = objective UnholyAlliance Cards.unholyAlliance

-- | Reveal 1 additional card from the encounter deck and add it to the staging area during
-- the staging step each round.
instance HasModifiersFor UnholyAlliance where
  getModifiersFor (UnholyAlliance a) = modified a GameTarget [AdditionalStagingReveals 1]

-- | If a hero is destroyed by an enemy attack, the players have lost the game.
instance RunMessage UnholyAlliance where
  runMessage msg (UnholyAlliance attrs) = case msg of
    CheckWindows ws | notNull [() | Window After (CharacterDestroyed (HeroTarget _) _) <- ws] -> do
      attacking <- attackDealingDamage
      when (isJust attacking) $ gameOver Lost
      UnholyAlliance <$> runMessage msg attrs
    _ -> UnholyAlliance <$> runMessage msg attrs
