module LordOfTheRings.Location.Cards.NecromancersPass (necromancersPass) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype NecromancersPass = NecromancersPass LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

necromancersPass :: LocationCard NecromancersPass
necromancersPass = location NecromancersPass Cards.necromancersPass

-- | Travel: The first player must discard 2 cards from his hand at random to travel here.
instance HasModifiersFor NecromancersPass where
  getModifiersFor (NecromancersPass a) = do
    hand <- length . (.hand) <$> (getPlayer =<< getLead)
    when (hand < 2) $ modified a a [CannotBeTraveledTo]

instance RunMessage NecromancersPass where
  runMessage msg l@(NecromancersPass a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      push $ DiscardRandomFromHand lead 2
      pure l
    _ -> NecromancersPass <$> runMessage msg a
