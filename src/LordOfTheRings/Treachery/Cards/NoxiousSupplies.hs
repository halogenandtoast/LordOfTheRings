module LordOfTheRings.Treachery.Cards.NoxiousSupplies (noxiousSupplies) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Log.Refs (cardTitle, sourceCardTitle)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel.Helpers (getWilyador)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoxiousSupplies = NoxiousSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

noxiousSupplies :: TreacheryCard NoxiousSupplies
noxiousSupplies = treachery NoxiousSupplies Cards.noxiousSupplies

-- | Counts as a Condition attachment with the text: "Forced: When attached character is
-- healed by a card effect other than Athelas, the amount of damage healed is reduced to 1.
-- Then, discard Noxious Supplies."
instance HasModifiersFor NoxiousSupplies where
  getModifiersFor (NoxiousSupplies a) = case a.placement of
    AttachedTo host -> modified a host [HealLimitExceptFrom 1 (toCardCode Objectives.athelas)]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to Wilyador.
instance RunMessage NoxiousSupplies where
  runMessage msg t@(NoxiousSupplies attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getWilyador >>= traverse_ (attachTreachery attrs.treacheryId)
      pure t
    -- read as the healing resolves, while the card healing is still where its title can be
    -- found, the limit cutting the same healing
    HealDamage source target n | AttachedTo host <- attrs.placement, target == host, n > 0 -> do
      g <- getsGame id
      damage <- getDamage host
      barred <- elem CannotBeHealed <$> getModifiers host
      let athelas = sourceCardTitle g source == Just (cardTitle Objectives.athelas)
      when (damage > 0 && not barred && not athelas) $ discard attrs attrs
      NoxiousSupplies <$> runMessage msg attrs
    _ -> NoxiousSupplies <$> runMessage msg attrs
