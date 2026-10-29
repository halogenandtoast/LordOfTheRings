module LordOfTheRings.Location.Cards.RavenVillage (ravenVillage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (dealRavenShadowCardsDuringAttack, revealRavenCard)

newtype RavenVillage = RavenVillage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenVillage :: LocationCard RavenVillage
ravenVillage = location RavenVillage Cards.ravenVillage

-- | Forced: After the last time counter is removed from this location, reveal the top card
-- of the Raven deck and add it to the staging area.
instance HasAbilities RavenVillage where
  getAbilities (RavenVillage a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Shadow: Deal attacking enemy 2 additional shadow cards from the Raven deck.
instance RunMessage RavenVillage where
  runMessage msg l@(RavenVillage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      revealRavenCard
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      dealRavenShadowCardsDuringAttack eid pid defenders 2
      pure l
    _ -> RavenVillage <$> runMessage msg a
