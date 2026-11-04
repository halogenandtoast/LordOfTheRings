module LordOfTheRings.Location.Cards.StreetsOfUlfast (streetsOfUlfast) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (ulfastX)

newtype StreetsOfUlfast = StreetsOfUlfast LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

streetsOfUlfast :: LocationCard StreetsOfUlfast
streetsOfUlfast = location StreetsOfUlfast Cards.streetsOfUlfast

-- | X is 1 more than the number of resources on the main quest: its threat.
instance HasModifiersFor StreetsOfUlfast where
  getModifiersFor (StreetsOfUlfast a) = do
    x <- ulfastX
    modifySelf a [ThreatModifier x]

-- | Travel: Raise each player's threat by X.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage StreetsOfUlfast where
  runMessage msg l@(StreetsOfUlfast a) = case msg of
    TravelTo lid | lid == a.id -> do
      x <- ulfastX
      eachPlayer \p -> encounterRaisesThreat p x
      StreetsOfUlfast <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> StreetsOfUlfast <$> runMessage msg a
