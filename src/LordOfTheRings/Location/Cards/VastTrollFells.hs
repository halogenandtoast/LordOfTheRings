module LordOfTheRings.Location.Cards.VastTrollFells (vastTrollFells) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isSafe)

newtype VastTrollFells = VastTrollFells LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

vastTrollFells :: LocationCard VastTrollFells
vastTrollFells = location VastTrollFells Cards.vastTrollFells

-- | Heroes cannot gain resources by card effects.
instance HasModifiersFor VastTrollFells where
  getModifiersFor (VastTrollFells a) = when (a.placement /= Unplaced) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotGainResourcesFromCardEffects]

-- | As an additional cost to travel to a safe location, each player must spend 1 resource.
-- Shadow: If no progress was placed on a side quest this round, attacking enemy gets +4
-- attack.
instance RunMessage VastTrollFells where
  runMessage msg l@(VastTrollFells a) = case msg of
    TravelTo lid | a.placement /= Unplaced -> do
      whenM (isSafe lid) $ eachPlayer \p -> spendResourceFromAHero p a
      VastTrollFells <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      sideQuests <- getSideQuests
      placed <- sum <$> traverse progressPlacedThisRound sideQuests
      when (placed == 0) $ enemyGetsAttack (CardIdSource cid) eid 4
      pure l
    _ -> VastTrollFells <$> runMessage msg a
