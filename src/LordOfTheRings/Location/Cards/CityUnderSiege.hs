module LordOfTheRings.Location.Cards.CityUnderSiege (cityUnderSiege) where

import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (guardingCards)

newtype CityUnderSiege = CityUnderSiege LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityUnderSiege :: LocationCard CityUnderSiege
cityUnderSiege = location CityUnderSiege Cards.cityUnderSiege

-- | While City Under Siege is in the staging area, it gains archery 2, and damage from the
-- archery keyword must be applied to Ship objectives.
-- ponytail: the archery damage is dealt as any other
instance HasModifiersFor CityUnderSiege where
  getModifiersFor (CityUnderSiege a) = when (a.placement == StagingArea) $ modifySelf a [GainKeyword (Archery 2)]

-- | Shadow: If attacking enemy is guarding an objective, return it to the staging area
-- after this attack.
instance RunMessage CityUnderSiege where
  runMessage msg l@(CityUnderSiege a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      guards <- guardingCards
      card <- toCard <$> getEnemy eid
      when (card.id `elem` guards) $ returnToStagingAfterAttack eid
      pure l
    _ -> CityUnderSiege <$> runMessage msg a
