module LordOfTheRings.Event.Cards.QuickerThanSight (quickerThanSight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype QuickerThanSight = QuickerThanSight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: Return a Silvan ally you control to your hand to cancel a shadow effect just
-- triggered during combat. The ally returned is its cost.
quickerThanSight :: EventCard QuickerThanSight
quickerThanSight =
  eventWith QuickerThanSight Cards.quickerThanSight \a ->
    a
      & windowL ?~ ShadowEffectWindow When
      & criteriaL .~ (shadowEffectsCancelable <> exists (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Silvan))

instance RunMessage QuickerThanSight where
  runMessage msg e@(QuickerThanSight attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      returnAllyToHand pid attrs (AllyWithTrait Silvan)
      cancelShadowEffects windows
      pure e
    _ -> QuickerThanSight <$> runMessage msg attrs
