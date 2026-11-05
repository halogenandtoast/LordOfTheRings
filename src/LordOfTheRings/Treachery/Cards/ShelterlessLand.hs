module LordOfTheRings.Treachery.Cards.ShelterlessLand (shelterlessLand) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShelterlessLand = ShelterlessLand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shelterlessLand :: TreacheryCard ShelterlessLand
shelterlessLand = treachery ShelterlessLand Cards.shelterlessLand

instance RunMessage ShelterlessLand where
  runMessage msg t@(ShelterlessLand a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      phaseModifier a GameTarget StagingThreatPerActiveLocationDamage
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      attackModifier (CardIdSource cid) eid AttackPerActiveLocationDamage
      pure t
    _ -> ShelterlessLand <$> runMessage msg a
