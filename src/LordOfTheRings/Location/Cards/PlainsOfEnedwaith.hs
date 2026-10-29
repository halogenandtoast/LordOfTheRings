module LordOfTheRings.Location.Cards.PlainsOfEnedwaith (plainsOfEnedwaith) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype PlainsOfEnedwaith = PlainsOfEnedwaith LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

plainsOfEnedwaith :: LocationCard PlainsOfEnedwaith
plainsOfEnedwaith = location PlainsOfEnedwaith Cards.plainsOfEnedwaith

-- | While Plains of Enedwaith is the active location, players do not draw a card during
-- the resource phase.
instance HasModifiersFor PlainsOfEnedwaith where
  getModifiersFor (PlainsOfEnedwaith a) = when (a.placement == ActiveLocation) do
    order <- getPlayerOrder
    for_ order \p -> modified a (PlayerTarget p) [ResourcePhaseDraws 0]

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if undefended).
instance RunMessage PlainsOfEnedwaith where
  runMessage msg l@(PlainsOfEnedwaith a) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      enemyGetsAttack a eid (ifUndefended defenders 3 1)
      pure l
    _ -> PlainsOfEnedwaith <$> runMessage msg a
