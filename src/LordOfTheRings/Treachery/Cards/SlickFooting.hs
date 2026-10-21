module LordOfTheRings.Treachery.Cards.SlickFooting (slickFooting) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SlickFooting = SlickFooting TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slickFooting :: TreacheryCard SlickFooting
slickFooting = treachery SlickFooting Cards.slickFooting

-- | When Revealed: Remove 1 progress token from each location in play. Then, discard the
-- top card of each player's deck for each progress token removed by this effect.
instance RunMessage SlickFooting where
  runMessage msg t@(SlickFooting attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      progressed <- filter ((> 0) . (.progress)) <$> (traverse getLocation =<< select AnyLocation)
      for_ progressed \l -> removeTokens attrs l Progress 1
      when (notNull progressed) $ eachPlayer \p -> discardTopOfDeck p (length progressed)
      pure t
    _ -> SlickFooting <$> runMessage msg attrs
