module LordOfTheRings.Location.Cards.PlainsOfRohan (plainsOfRohan) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype PlainsOfRohan = PlainsOfRohan LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

plainsOfRohan :: LocationCard PlainsOfRohan
plainsOfRohan = location PlainsOfRohan Cards.plainsOfRohan

-- | X is the number of locations in the staging area.
instance HasModifiersFor PlainsOfRohan where
  getModifiersFor (PlainsOfRohan a) = do
    x <- length <$> select LocationInStagingArea
    modifySelf a [ThreatModifier x]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, increase
-- the pursuit value by 1.
instance RunMessage PlainsOfRohan where
  runMessage msg l@(PlainsOfRohan a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      increasePursuit 1
      pure l
    _ -> PlainsOfRohan <$> runMessage msg a
