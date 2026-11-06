module LordOfTheRings.Treachery.Cards.LostAndWitless (lostAndWitless) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))

newtype LostAndWitless = LostAndWitless TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostAndWitless :: TreacheryCard LostAndWitless
lostAndWitless = treachery LostAndWitless Cards.lostAndWitless

instance RunMessage LostAndWitless where
  runMessage msg t@(LostAndWitless a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      locations <- select LocationInStagingArea
      for_ locations \lid -> do
        location <- getLocation lid
        removeTokens a lid Progress location.progress
      active <- select (IsActiveLocation <> NotLocation UniqueLocation)
      for_ (headMay active) \current -> do
        replacement <- headMay <$> shuffleM locations
        for_ replacement \lid -> do
          placeLocation current StagingArea
          placeLocation lid ActiveLocation
      pure t
    _ -> LostAndWitless <$> runMessage msg a
