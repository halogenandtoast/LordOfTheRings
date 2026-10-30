module LordOfTheRings.Location.Cards.SouthernBelfalas (southernBelfalas) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SouthernBelfalas = SouthernBelfalas LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southernBelfalas :: LocationCard SouthernBelfalas
southernBelfalas = location SouthernBelfalas Cards.southernBelfalas

-- | Travel: One player must reveal the top card of the Corsair deck and engage it to travel
-- here.
-- Shadow: If this attack destroys a character, discard an ally controlled by the defending
-- player.
instance RunMessage SouthernBelfalas where
  runMessage msg l@(SouthernBelfalas a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      choosePlayerM lead a (revealFromCorsairDeck a)
      SouthernBelfalas <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "southernBelfalas.discard"
        causedBy a
        targets allies discard
      pure l
    _ -> SouthernBelfalas <$> runMessage msg a
