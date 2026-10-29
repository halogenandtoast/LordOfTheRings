module LordOfTheRings.Location.Cards.FordsOfIsen (fordsOfIsen) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype FordsOfIsen = FordsOfIsen LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

fordsOfIsen :: LocationCard FordsOfIsen
fordsOfIsen = location FordsOfIsen Cards.fordsOfIsen

-- | While Fords of Isen is in the staging area, players cannot gain resources from card
-- effects.
instance HasModifiersFor FordsOfIsen where
  getModifiersFor (FordsOfIsen a) = when (a.placement == StagingArea) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotGainResourcesFromCardEffects]

-- | Forced: After Fords of Isen becomes the active location, each player with fewer than 5
-- cards in his hand draws cards until he has 5 in his hand.
instance HasAbilities FordsOfIsen where
  getAbilities (FordsOfIsen a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage FordsOfIsen where
  runMessage msg l@(FordsOfIsen a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        hand <- length . (.hand) <$> getPlayer p
        when (hand < 5) $ drawCards p (5 - hand)
      pure l
    _ -> FordsOfIsen <$> runMessage msg a
