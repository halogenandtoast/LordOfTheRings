module LordOfTheRings.Ally.Cards.SquireOfTheCitadel (squireOfTheCitadel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype SquireOfTheCitadel = SquireOfTheCitadel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

squireOfTheCitadel :: AllyCard SquireOfTheCitadel
squireOfTheCitadel = ally SquireOfTheCitadel Cards.squireOfTheCitadel

-- | Response: After Squire of the Citadel leaves play, add 1 resource to a Gondor hero's
-- resource pool.
instance HasAbilities SquireOfTheCitadel where
  getAbilities (SquireOfTheCitadel a) =
    [restricted a 1 (exists $ HeroWithTrait Gondor) $ response $ LeftPlayWindow After (toTarget a)]

instance RunMessage SquireOfTheCitadel where
  runMessage msg s@(SquireOfTheCitadel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select (HeroWithTrait Gondor)
      chooseOrRunOneM pid do
        questionLabeled "squireOfTheCitadel.chooseHero"
        causedBy a
        targets heroes \h -> placeResources a h 1
      pure s
    _ -> SquireOfTheCitadel <$> runMessage msg a
