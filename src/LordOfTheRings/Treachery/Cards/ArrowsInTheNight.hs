module LordOfTheRings.Treachery.Cards.ArrowsInTheNight (arrowsInTheNight) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ArrowsInTheNight = ArrowsInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

arrowsInTheNight :: TreacheryCard ArrowsInTheNight
arrowsInTheNight = treachery ArrowsInTheNight Cards.arrowsInTheNight

instance RunMessage ArrowsInTheNight where
  runMessage msg t@(ArrowsInTheNight a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      roundModifier a GameTarget (ArcheryBonusAtStage you 6)
      discardAnyFromHandWithResult a you
      pure t
    DiscardedChosenFromHand (isSource a -> True) pid cards -> do
      when (notNull cards) $ roundModifier a GameTarget (ArcheryBonusAtStage pid $ negate $ length cards)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardRandomFromHand pid 1
      pure t
    _ -> ArrowsInTheNight <$> runMessage msg a
