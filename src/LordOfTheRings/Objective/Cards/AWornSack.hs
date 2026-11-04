module LordOfTheRings.Objective.Cards.AWornSack (aWornSack) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (sackModifiers, sackOntoHighest, sackableHeroes)

newtype AWornSack = AWornSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aWornSack :: ObjectiveCard AWornSack
aWornSack = objective AWornSack Cards.aWornSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor AWornSack where
  getModifiersFor (AWornSack a) = sackModifiers a

-- | When Sacked: Attach to hero with the most resources without a Sack attached.
instance RunMessage AWornSack where
  runMessage msg o@(AWornSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableHeroes
      sackOntoHighest attrs =<< traverse (\c -> (c,) <$> resourcesOf c) characters
      pure o
    _ -> AWornSack <$> runMessage msg attrs
   where
    resourcesOf = \case
      HeroTarget hid -> (.resources) <$> getHero hid
      _ -> pure 0
