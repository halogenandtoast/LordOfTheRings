module LordOfTheRings.Location.Cards.Waterspout (waterspout) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Waterspout = Waterspout LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

waterspout :: LocationCard Waterspout
waterspout = location Waterspout Cards.waterspout

-- | While your heading is at the worst setting, when faced with the option to travel, the
-- players must travel to Waterspout, if able.
instance HasModifiersFor Waterspout where
  getModifiersFor (Waterspout a) = whenM atWorstHeading $ modified a a [MustBeTraveledTo]

-- | Forced: After traveling to Waterspout, deal 4 damage to each Ship objective.
instance HasAbilities Waterspout where
  getAbilities (Waterspout a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage Waterspout where
  runMessage msg l@(Waterspout a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ships <- select shipObjective
      for_ ships \s -> dealDamage a s 4
      pure l
    _ -> Waterspout <$> runMessage msg a
