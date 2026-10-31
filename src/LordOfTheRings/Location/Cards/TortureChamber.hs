module LordOfTheRings.Location.Cards.TortureChamber (tortureChamber) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Trait (Trait (DolGuldur))

newtype TortureChamber = TortureChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tortureChamber :: LocationCard TortureChamber
tortureChamber = location TortureChamber Cards.tortureChamber

-- | Forced: At the end of each round, place 1 resource on each Dol Guldur location in play.
instance HasAbilities TortureChamber where
  getAbilities (TortureChamber a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | If Torture Chamber has 4 or more resource tokens on it, all "prisoners" were killed,
-- and the players have lost the game.
instance RunMessage TortureChamber where
  runMessage msg l@(TortureChamber a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      locations <- select (LocationWithTrait DolGuldur)
      for_ locations \loc -> placeResources a loc 1
      pure l
    PlaceTokens _ (isTarget a -> True) Resource n | countTokens Resource a.locationTokens + n >= 4 -> do
      gameOver Lost
      TortureChamber <$> runMessage msg a
    _ -> TortureChamber <$> runMessage msg a
