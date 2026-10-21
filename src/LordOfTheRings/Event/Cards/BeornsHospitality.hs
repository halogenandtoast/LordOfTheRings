module LordOfTheRings.Event.Cards.BeornsHospitality (beornsHospitality) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype BeornsHospitality = BeornsHospitality EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. Heal all damage on each hero controlled by that player.
beornsHospitality :: EventCard BeornsHospitality
beornsHospitality = event BeornsHospitality Cards.beornsHospitality

instance RunMessage BeornsHospitality where
  runMessage msg e@(BeornsHospitality attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        for_ heroes $ healAllDamage attrs
      pure e
    _ -> BeornsHospitality <$> runMessage msg attrs
