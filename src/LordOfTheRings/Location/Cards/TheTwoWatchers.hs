module LordOfTheRings.Location.Cards.TheTwoWatchers (theTwoWatchers) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheTwoWatchers = TheTwoWatchers LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The location side of The Two Watchers; its enemy side is 'Cards.theTwoWatchers'' other
-- side.
theTwoWatchers :: LocationCard TheTwoWatchers
theTwoWatchers = location TheTwoWatchers Cards.theTwoWatchers

-- | Immune to player card effects. X is twice the stage number of the main quest: its
-- threat, printed X.
instance HasModifiersFor TheTwoWatchers where
  getModifiersFor (TheTwoWatchers a) = do
    x <- (* 2) <$> getMainStage
    modifySelf a $ ImmuneToPlayerCardEffects : [ThreatModifier x | x > 0]

-- | Forced: After the players travel here, flip The Two Watchers to its enemy side and move
-- it to the staging area.
instance HasAbilities TheTwoWatchers where
  getAbilities (TheTwoWatchers a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage TheTwoWatchers where
  runMessage msg l@(TheTwoWatchers a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    _ -> TheTwoWatchers <$> runMessage msg a
