module LordOfTheRings.Objective.Cards.ADeepSack (aDeepSack) where

import LordOfTheRings.Card (CardDef (cdWillpower))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (printedStat, sackModifiers, sackOntoHighest, sackableCharacters)

newtype ADeepSack = ADeepSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aDeepSack :: ObjectiveCard ADeepSack
aDeepSack = objective ADeepSack Cards.aDeepSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor ADeepSack where
  getModifiersFor (ADeepSack a) = sackModifiers a

-- | When Sacked: Attach to the character (excluding Gandalf) with the highest printed willpower without a Sack attached.
instance RunMessage ADeepSack where
  runMessage msg o@(ADeepSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableCharacters
      sackOntoHighest attrs =<< traverse (\c -> (c,) <$> printedStat cdWillpower c) characters
      pure o
    _ -> ADeepSack <$> runMessage msg attrs
