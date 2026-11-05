module LordOfTheRings.Location.Cards.FesteringMire (festeringMire) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)

newtype FesteringMire = FesteringMire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

festeringMire :: LocationCard FesteringMire
festeringMire = location FesteringMire Cards.festeringMire

instance HasModifiersFor FesteringMire where
  getModifiersFor (FesteringMire a) = do
    unable <- anyM (\pid -> do
      hand <- discardableHand <$> getPlayer pid
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      pure $ null hand || barred) =<< getRemainingPlayers
    when unable $ modifySelf a [CannotBeTraveledTo]

instance HasAbilities FesteringMire where
  getAbilities (FesteringMire a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

instance RunMessage FesteringMire where
  runMessage msg l@(FesteringMire a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> discardRandomFromHand pid 1
      FesteringMire <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer discardHand
      pure l
    _ -> FesteringMire <$> runMessage msg a
