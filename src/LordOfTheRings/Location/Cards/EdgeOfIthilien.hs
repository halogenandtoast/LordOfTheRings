module LordOfTheRings.Location.Cards.EdgeOfIthilien (edgeOfIthilien) where

import LordOfTheRings.Card (CardType (TreacheryType), cdCardType, toCardDef)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, turnFaceup)

newtype EdgeOfIthilien = EdgeOfIthilien LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

edgeOfIthilien :: LocationCard EdgeOfIthilien
edgeOfIthilien = location EdgeOfIthilien Cards.edgeOfIthilien

-- | Archery X. X is the highest number of hidden cards in any player's play area.
instance HasModifiersFor EdgeOfIthilien where
  getModifiersFor (EdgeOfIthilien a) = do
    x <- foldr max 0 <$> (traverse (fmap length . getHiddenCards) =<< getRemainingPlayers)
    when (x > 0) $ modified a a [GainKeyword (Archery x)]

-- | Shadow: Turn 1 hidden card in your play area faceup. If it is a treachery card,
-- attacking enemy makes an additional attack after this one. The first taken turns: they
-- are facedown, so any is as good as another.
instance RunMessage EdgeOfIthilien where
  runMessage msg l@(EdgeOfIthilien a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      hidden <- getHiddenCards pid
      for_ (take 1 hidden) \card -> do
        _ <- turnFaceup pid card
        when (cdCardType (toCardDef card) == TreacheryType) $ additionalAttack pid eid
      pure l
    _ -> EdgeOfIthilien <$> runMessage msg a
