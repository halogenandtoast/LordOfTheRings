module LordOfTheRings.Location.Cards.BurningPiers (burningPiers) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype BurningPiers = BurningPiers LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

burningPiers :: LocationCard BurningPiers
burningPiers = location BurningPiers Cards.burningPiers

-- | Burning Piers gets +1 threat for each damage on it.
instance HasModifiersFor BurningPiers where
  getModifiersFor (BurningPiers a) = when (a.damage > 0) $ modifySelf a [ThreatModifier a.damage]

-- | Forced: When Burning Piers is placed underneath The Havens Burn, deal 3 damage to The
-- Dream-chaser.
-- Shadow: Add Burning Piers to the staging area.
instance RunMessage BurningPiers where
  runMessage msg l@(BurningPiers a) = case msg of
    PlaceUnderneath _ cards | any ((== a.cardId) . (.id)) cards -> do
      dreamChaser <- select (LocationWithCardCode $ toCardCode Cards.dreamChaserLocation)
      for_ dreamChaser \d -> dealDamage a d 3
      BurningPiers <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      addShadowCardToStaging eid (MkEncounterCard cid (toCardCode a))
      pure l
    _ -> BurningPiers <$> runMessage msg a
