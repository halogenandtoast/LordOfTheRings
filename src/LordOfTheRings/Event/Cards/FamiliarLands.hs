module LordOfTheRings.Event.Cards.FamiliarLands (familiarLands) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype FamiliarLands = FamiliarLands EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

familiarLands :: EventCard FamiliarLands
familiarLands =
  eventWith FamiliarLands Cards.familiarLands $ criteriaL .~ exists LocationWithPlayerCardAttachment

-- | Action: Each location with a player card attachment gets -2 [threat] until the end of
-- the phase.
instance RunMessage FamiliarLands where
  runMessage msg e@(FamiliarLands attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      locations <- select LocationWithPlayerCardAttachment
      for_ locations \l -> phaseModifier attrs l (ThreatModifier (-2))
      pure e
    _ -> FamiliarLands <$> runMessage msg attrs
