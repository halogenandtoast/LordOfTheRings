module LordOfTheRings.Ally.Cards.SquireOfTheCitadel (squireOfTheCitadel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Gondor))

newtype SquireOfTheCitadel = SquireOfTheCitadel AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

squireOfTheCitadel :: AllyCard SquireOfTheCitadel
squireOfTheCitadel = ally SquireOfTheCitadel Cards.squireOfTheCitadel

-- | Response: After Squire of the Citadel leaves play, add 1 resource to a Gondor hero's
-- resource pool.
--
-- ponytail: offered as it leaves, not as a response in the left-play window: abilities
-- are only read off cards in play. A window for cards that just left play would fix it.
instance RunMessage SquireOfTheCitadel where
  runMessage msg (SquireOfTheCitadel a) = case msg of
    _
      | Just pid <- placementController a.placement
      , leaving msg -> do
          heroes <- select (HeroWithTrait Gondor)
          chooseOneM pid do
            questionLabeled "squireOfTheCitadel.chooseHero"
            targets heroes \h -> placeResources a h 1
            labeled "pass" (pure ())
          SquireOfTheCitadel <$> runMessage msg a
    _ -> SquireOfTheCitadel <$> runMessage msg a
   where
    leaving = \case
      Discard t -> isTarget a t
      ReturnToHand t -> isTarget a t
      ShuffleIntoOwnersDeck t -> isTarget a t
      _ -> False
