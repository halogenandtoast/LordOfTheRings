module LordOfTheRings.Objective.Cards.ALargeSack (aLargeSack) where

import LordOfTheRings.Card (CardDef (cdHitPoints))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (printedStat, sackModifiers, sackOntoHighest, sackableCharacters)

newtype ALargeSack = ALargeSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aLargeSack :: ObjectiveCard ALargeSack
aLargeSack = objective ALargeSack Cards.aLargeSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor ALargeSack where
  getModifiersFor (ALargeSack a) = sackModifiers a

-- | When Sacked: Attach to the character (excluding Gandalf) with the highest printed hit points without a Sack attached.
instance RunMessage ALargeSack where
  runMessage msg o@(ALargeSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableCharacters
      sackOntoHighest attrs =<< traverse (\c -> (c,) <$> printedStat cdHitPoints c) characters
      pure o
    _ -> ALargeSack <$> runMessage msg attrs
