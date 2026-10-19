module LordOfTheRings.Event.Cards.BeornsHospitality (beornsHospitality) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype BeornsHospitality = BeornsHospitality EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. Heal all damage on each hero controlled by that player.
beornsHospitality :: EventCard BeornsHospitality
beornsHospitality = event BeornsHospitality Cards.beornsHospitality

instance RunMessage BeornsHospitality where
  runMessage msg e@(BeornsHospitality attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      order <- getPlayerOrder
      heals <- for order \p -> do
        heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId p)
        (p,) <$> for heroes \h -> HealDamage (toSource attrs) h <$> getDamage h
      push =<< choosePlayer pid (Just $ toTarget attrs) \p -> fromMaybe [] (lookup p heals)
      pure e
    _ -> BeornsHospitality <$> runMessage msg attrs
