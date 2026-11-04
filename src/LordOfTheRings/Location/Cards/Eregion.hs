module LordOfTheRings.Location.Cards.Eregion (eregion) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (discardAlliesAsGroup)

newtype Eregion = Eregion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eregion :: LocationCard Eregion
eregion = location Eregion Cards.eregion

-- | Forced: When Eregion is explored, the players as a group must discard X allies from
-- play. X is the number of damage here.
instance HasAbilities Eregion where
  getAbilities (Eregion a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: Defending player discards a non-objective attachment he controls.
instance RunMessage Eregion where
  runMessage msg l@(Eregion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardAlliesAsGroup a a.damage
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardNonObjectiveAttachment a pid
      pure l
    _ -> Eregion <$> runMessage msg a
