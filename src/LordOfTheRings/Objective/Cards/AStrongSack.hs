module LordOfTheRings.Objective.Cards.AStrongSack (aStrongSack) where

import LordOfTheRings.Card (CardDef (cdAttack))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (printedStat, sackModifiers, sackOntoHighest, sackableCharacters)

newtype AStrongSack = AStrongSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aStrongSack :: ObjectiveCard AStrongSack
aStrongSack = objective AStrongSack Cards.aStrongSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor AStrongSack where
  getModifiersFor (AStrongSack a) = sackModifiers a

-- | When Sacked: Attach to the character (excluding Gandalf) with the highest printed attack without a Sack attached.
instance RunMessage AStrongSack where
  runMessage msg o@(AStrongSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableCharacters
      sackOntoHighest attrs =<< traverse (\c -> (c,) <$> printedStat cdAttack c) characters
      pure o
    _ -> AStrongSack <$> runMessage msg attrs
