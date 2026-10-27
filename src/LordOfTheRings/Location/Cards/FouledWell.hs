module LordOfTheRings.Location.Cards.FouledWell (fouledWell) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FouledWell = FouledWell LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fouledWell :: LocationCard FouledWell
fouledWell = location FouledWell Cards.fouledWell

-- | When Revealed: Each player may choose and discard 1 card at random from his hand. If
-- all players did not discard 1 card, Fouled Well gains Surge.
-- Once a player declines (or has no card to discard), the surge is certain, so the
-- players after him are not asked.
instance RunMessage FouledWell where
  runMessage msg l@(FouledWell a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      askEach a =<< getRemainingPlayers
      pure l
    _ -> FouledWell <$> runMessage msg a
   where
    askEach :: ReverseQueue m => LocationAttrs -> [PlayerId] -> m ()
    askEach _ [] = pure ()
    askEach attrs (p : rest) = do
      hand <- (.hand) <$> getPlayer p
      if null hand
        then revealEncounterCard
        else chooseOneM p do
          questionLabeled "fouledWell.discard"
          causedBy attrs
          labeled "fouledWell.discardRandom" do
            discardRandomFromHand p 1
            askEach attrs rest
          labeled "fouledWell.decline" revealEncounterCard
