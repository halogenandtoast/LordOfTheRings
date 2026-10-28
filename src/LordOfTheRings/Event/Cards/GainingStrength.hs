module LordOfTheRings.Event.Cards.GainingStrength (gainingStrength) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype GainingStrength = GainingStrength EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Discard 2 resources from your hero's resource pool to add 3 resources to that
-- hero's resource pool.
gainingStrength :: EventCard GainingStrength
gainingStrength =
  eventWith GainingStrength Cards.gainingStrength \a ->
    a & criteriaL .~ exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithResourcesAtLeast 2)

instance RunMessage GainingStrength where
  runMessage msg e@(GainingStrength attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResourcesAtLeast 2)
      chooseOrRunOneM pid do
        questionLabeled "gainingStrength.chooseHero"
        causedBy attrs
        targets heroes \h -> do
          removeTokens attrs h Resource 2
          placeResources attrs h 3
      pure e
    _ -> GainingStrength <$> runMessage msg attrs
