module LordOfTheRings.Treachery.Cards.DunlendingAmbush (dunlendingAmbush) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheDunlandTrap.Helpers (eachPlayerSearchesToEngage)
import LordOfTheRings.Trait (Trait (Dunland))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DunlendingAmbush = DunlendingAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlendingAmbush :: TreacheryCard DunlendingAmbush
dunlendingAmbush = treachery DunlendingAmbush Cards.dunlendingAmbush

-- | Counts as a Condition attachment with the text: "Forced: After attached location is
-- explored, each player searches the encounter deck and discard pile for a Dunland enemy
-- and puts it into play engaged with him. Shuffle the encounter deck."
instance HasAbilities DunlendingAmbush where
  getAbilities (DunlendingAmbush a) = case a.placement of
    AttachedTo (LocationTarget lid) -> [mkAbility a 1 $ forced $ ExploredWindow When lid]
    _ -> []

-- | When Revealed: Attach to the active location. (Limit 1 per location: with a copy
-- already there, or no active location, it is discarded.)
instance RunMessage DunlendingAmbush where
  runMessage msg t@(DunlendingAmbush attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select IsActiveLocation
      copies <- getsGame (filter ((== toCardCode attrs) . toCardCode) . toList . (.entities.treacheries))
      for_ (take 1 active) \l ->
        unless (any ((== AttachedTo (toTarget l)) . (.placement)) copies)
          $ attachTreachery attrs.treacheryId l
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayerSearchesToEngage attrs Dunland
      pure t
    _ -> DunlendingAmbush <$> runMessage msg attrs
