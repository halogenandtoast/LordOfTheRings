module LordOfTheRings.Ally.Cards.GhanBuriGhan (ghanBuriGhan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Placement (placementController)

newtype GhanBuriGhan = GhanBuriGhan AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

ghanBuriGhan :: AllyCard GhanBuriGhan
ghanBuriGhan = ally GhanBuriGhan Cards.ghanBuriGhan

-- | X is the threat of the active location: his willpower.
instance HasModifiersFor GhanBuriGhan where
  getModifiersFor (GhanBuriGhan a) = do
    x <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
    when (x > 0) $ modifySelf a [WillpowerModifier x]

-- | Travel Action: Discard Ghân-buri-Ghân to travel to a location without resolving its
-- Travel effect. Traveling as the players would, there must be no active location.
instance HasAbilities GhanBuriGhan where
  getAbilities (GhanBuriGhan a) =
    [ restricted a 1 (DuringPhase TravelPhase <> NotCriterion (exists IsActiveLocation) <> exists LocationInStagingArea)
        $ ActionAbility Free
    | Just _ <- [placementController a.placement]
    ]

instance RunMessage GhanBuriGhan where
  runMessage msg s@(GhanBuriGhan a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      locations <- travelableLocations
      chooseOrRunOneM pid do
        questionLabeled "ghanBuriGhan.chooseLocation"
        causedBy a
        targets locations travelWithoutCost
      pure s
    _ -> GhanBuriGhan <$> runMessage msg a
