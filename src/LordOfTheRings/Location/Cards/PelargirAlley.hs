module LordOfTheRings.Location.Cards.PelargirAlley (pelargirAlley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (getScroll, scrollHolder, scrollOnHero)

newtype PelargirAlley = PelargirAlley LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirAlley :: LocationCard PelargirAlley
pelargirAlley = location PelargirAlley Cards.pelargirAlley

-- | While Alcaron's Scroll is not attached to a hero, Pelargir Alley gets +3 threat.
instance HasModifiersFor PelargirAlley where
  getModifiersFor (PelargirAlley a) = do
    onHero <- scrollOnHero
    unless onHero $ modified a a [ThreatModifier 3]

-- | Shadow: If the defending player controls a hero with Alcaron's Scroll attached, attach
-- Alcaron's Scroll to the attacking enemy.
instance RunMessage PelargirAlley where
  runMessage msg l@(PelargirAlley a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      holder <- scrollHolder
      when (holder == Just pid) $ traverse_ (`attachObjective` eid) =<< getScroll
      pure l
    _ -> PelargirAlley <$> runMessage msg a
