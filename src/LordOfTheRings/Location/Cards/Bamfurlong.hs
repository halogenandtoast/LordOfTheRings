module LordOfTheRings.Location.Cards.Bamfurlong (bamfurlong) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Nazgul))

newtype Bamfurlong = Bamfurlong LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bamfurlong :: LocationCard Bamfurlong
bamfurlong = location Bamfurlong Cards.bamfurlong

-- | Response: After the players travel to Bamfurlong, each player engaged with a Nazgûl
-- enemy may ready a hero he controls.
instance HasAbilities Bamfurlong where
  getAbilities (Bamfurlong a) =
    [restricted a 1 (PlayerExists $ PlayerEngagedWith $ EnemyWithTrait Nazgul) $ response $ TraveledWindow After a.id]

instance RunMessage Bamfurlong where
  runMessage msg l@(Bamfurlong a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      engaged <- select (PlayerEngagedWith $ EnemyWithTrait Nazgul)
      order <- getRemainingPlayers
      for_ (filter (`elem` engaged) order) \p -> do
        heroes <- select (HeroControlledBy (PlayerWithId p) <> ExhaustedHero)
        unless (null heroes) $ chooseOneM p do
          questionLabeled "bamfurlong.ready"
          causedBy a
          targets heroes ready
          labeled "pass" $ pure ()
      pure l
    _ -> Bamfurlong <$> runMessage msg a
