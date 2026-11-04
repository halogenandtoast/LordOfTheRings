module LordOfTheRings.Treachery.Cards.ClosingIn (closingIn) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch, placeUnderOrcsSearch)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ClosingIn = ClosingIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

closingIn :: TreacheryCard ClosingIn
closingIn = treachery ClosingIn Cards.closingIn

-- | Surge.
-- When Revealed: Place Closing In facedown underneath The Orcs' Search.
-- Shadow: Attacking enemy gets +1 attack for each card underneath The Orcs' Search.
--
instance RunMessage ClosingIn where
  runMessage msg t@(ClosingIn attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      removeFromGame attrs
      placeUnderOrcsSearch $ EncounterCard $ MkEncounterCard attrs.cardId (toCardCode attrs)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- cardsUnderOrcsSearch
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> ClosingIn <$> runMessage msg attrs
