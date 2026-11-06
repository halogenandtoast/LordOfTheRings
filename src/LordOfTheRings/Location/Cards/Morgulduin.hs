module LordOfTheRings.Location.Cards.Morgulduin (morgulduin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype Morgulduin = Morgulduin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulduin :: LocationCard Morgulduin
morgulduin = location Morgulduin Cards.morgulduin

instance HasAbilities Morgulduin where
  getAbilities (Morgulduin a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow When | a.placement == ActiveLocation]

instance RunMessage Morgulduin where
  runMessage msg l@(Morgulduin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [target | Window _ (CommittedToQuest target) <- ws] \target -> dealDamage attrs target 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      crossed <- crossedAnduin
      unless crossed $ putShadowLocationIntoPlay eid cid
      pure l
    _ -> Morgulduin <$> runMessage msg attrs
