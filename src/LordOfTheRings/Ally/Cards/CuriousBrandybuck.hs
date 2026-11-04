module LordOfTheRings.Ally.Cards.CuriousBrandybuck (curiousBrandybuck) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (PlayerCard (..), toCardCode)

newtype CuriousBrandybuck = CuriousBrandybuck AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Response: After you travel to a location, put Curious Brandybuck into play from your
-- hand, under any player's control.
curiousBrandybuck :: AllyCard CuriousBrandybuck
curiousBrandybuck = ally (\a -> CuriousBrandybuck a {allyHandWindow = Just $ AnyTraveledWindow After}) Cards.curiousBrandybuck

-- | Forced: After the active location is explored, place Curious Brandybuck on the bottom
-- of its owner's deck.
instance HasAbilities CuriousBrandybuck where
  getAbilities (CuriousBrandybuck a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After]

instance RunMessage CuriousBrandybuck where
  runMessage msg s@(CuriousBrandybuck a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      choosePlayerM pid a \p -> putAllyIntoPlayUnder p (MkPlayerCard cid (toCardCode a) (Just a.owner))
      pure s
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      putOnBottomOfOwnersDeck a
      pure s
    _ -> CuriousBrandybuck <$> runMessage msg a
