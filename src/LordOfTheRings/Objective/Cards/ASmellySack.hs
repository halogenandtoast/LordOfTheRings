module LordOfTheRings.Objective.Cards.ASmellySack (aSmellySack) where

import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (sackModifiers, sackOntoHighest, sackableAllies)

newtype ASmellySack = ASmellySack ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aSmellySack :: ObjectiveCard ASmellySack
aSmellySack = objective ASmellySack Cards.aSmellySack

-- | Attached character cannot ready, attack, defend, commit to quests, or trigger effects.
-- If this Sack card is removed, shuffle it into the sack deck.
instance HasModifiersFor ASmellySack where
  getModifiersFor (ASmellySack a) = sackModifiers a

-- | When Sacked: Attach to the ally (excluding Gandalf) with the highest printed cost without a Sack attached.
instance RunMessage ASmellySack where
  runMessage msg o@(ASmellySack attrs) = case msg of
    WhenSacked oid | oid == attrs.id -> do
      characters <- sackableAllies
      sackOntoHighest attrs =<< traverse (\c -> (c,) . printedCostOf <$> characterCardDef c) characters
      pure o
    _ -> ASmellySack <$> runMessage msg attrs
