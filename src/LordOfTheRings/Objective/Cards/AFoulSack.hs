module LordOfTheRings.Objective.Cards.AFoulSack (aFoulSack) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (sackModifiers, sackOntoHighest, sackableCharacters)

newtype AFoulSack = AFoulSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFoulSack :: ObjectiveCard AFoulSack
aFoulSack = objective AFoulSack Cards.aFoulSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor AFoulSack where
  getModifiersFor (AFoulSack a) = sackModifiers a

-- | When Sacked: Attach to the character (excluding Gandalf) with the most attachments without a Sack attached.
instance RunMessage AFoulSack where
  runMessage msg o@(AFoulSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableCharacters
      sackOntoHighest attrs =<< traverse (\c -> (c,) . length <$> getAllAttachmentsOn c) characters
      pure o
    _ -> AFoulSack <$> runMessage msg attrs
