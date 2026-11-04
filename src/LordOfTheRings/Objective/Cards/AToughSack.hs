module LordOfTheRings.Objective.Cards.AToughSack (aToughSack) where

import LordOfTheRings.Card (CardDef (cdDefense))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (printedStat, sackModifiers, sackOntoHighest, sackableCharacters)

newtype AToughSack = AToughSack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aToughSack :: ObjectiveCard AToughSack
aToughSack = objective AToughSack Cards.aToughSack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor AToughSack where
  getModifiersFor (AToughSack a) = sackModifiers a

-- | When Sacked: Attach to the character (excluding Gandalf) with the highest printed defense without a Sack attached.
instance RunMessage AToughSack where
  runMessage msg o@(AToughSack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableCharacters
      sackOntoHighest attrs =<< traverse (\c -> (c,) <$> printedStat cdDefense c) characters
      pure o
    _ -> AToughSack <$> runMessage msg attrs
