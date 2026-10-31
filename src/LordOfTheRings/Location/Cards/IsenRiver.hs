module LordOfTheRings.Location.Cards.IsenRiver (isenRiver) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype IsenRiver = IsenRiver LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

isenRiver :: LocationCard IsenRiver
isenRiver = location IsenRiver Cards.isenRiver

-- | While Isen River is in the staging area, each player cannot discard cards from his hand.
instance HasModifiersFor IsenRiver where
  getModifiersFor (IsenRiver a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDiscardFromHand]

-- | Forced: After Isen River becomes the active location, each player draws a card.
instance HasAbilities IsenRiver where
  getAbilities (IsenRiver a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage IsenRiver where
  runMessage msg l@(IsenRiver a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> drawCards a p 1
      pure l
    _ -> IsenRiver <$> runMessage msg a
