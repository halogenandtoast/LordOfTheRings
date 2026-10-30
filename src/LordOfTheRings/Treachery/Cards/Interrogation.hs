module LordOfTheRings.Treachery.Cards.Interrogation (interrogation) where

import LordOfTheRings.Card (Card (PlayerCard), CardDef (cdCost), costAsResources, toCardDef)
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (
  capturedDeck,
  discardTopOfCapturedDeck,
  shuffleFromHandIntoCapturedDeck,
 )
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Interrogation = Interrogation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

interrogation :: TreacheryCard Interrogation
interrogation = treachery Interrogation Cards.interrogation

-- | When Revealed: Each player discards the top card of his captured deck and raises his
-- threat by that card's printed resource cost. If at least 1 player discarded no cards from
-- this effect, Interrogation gains doomed 2. (A hero's printed cost is 0.)
-- Shadow: Shuffle 2 random cards from your hand into your captured deck.
instance RunMessage Interrogation where
  runMessage msg t@(Interrogation attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      tops <- for players \pid -> (pid,) . headMay <$> capturedDeck pid
      for_ tops \(pid, top) -> for_ top \card -> do
        discardTopOfCapturedDeck pid
        encounterRaisesThreat pid $ maybe 0 costAsResources (cdCost $ toCardDef card)
      when (any (isNothing . snd) tops) $ raiseThreatFromDoomed 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      hand <- (.hand) <$> getPlayer pid
      chosen <- take 2 <$> shuffleM [pc | PlayerCard pc <- hand]
      shuffleFromHandIntoCapturedDeck pid chosen
      pure t
    _ -> Interrogation <$> runMessage msg attrs
