module LordOfTheRings.Treachery.Cards.LightningSplinters (lightningSplinters) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LightningSplinters = LightningSplinters TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lightningSplinters :: TreacheryCard LightningSplinters
lightningSplinters = treachery LightningSplinters Cards.lightningSplinters

instance RunMessage LightningSplinters where
  runMessage msg t@(LightningSplinters attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discardForPlayers attrs [(p, 3, []) | p <- players] []
      pure t
    WithTarget (isTarget attrs -> True) (ScenarioSpecific "lightningSplinters.discard" value)
      | Just (pending, done) <- parseMaybe parseJSON value -> do
          discardForPlayers attrs pending done
          pure t
    _ -> LightningSplinters <$> runMessage msg attrs

{- | All players discard before any player assigns damage. Keep the chosen cards in the
queued continuation so their printed costs survive discard responses and saved games.
-}
discardForPlayers
  :: ReverseQueue m => TreacheryAttrs -> [(PlayerId, Int, [Card])] -> [(PlayerId, [Card])] -> m ()
discardForPlayers attrs pending done = case pending of
  [] -> for_ done \(p, cards) -> do
    let n = sum $ map printedCostOf cards
    when (n > 0) $ dealDamageDivided attrs p n
  (p, n, cards) : rest -> do
    hand <- discardableHand <$> getPlayer p
    barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget p)
    if n <= 0 || null hand || barred
      then discardForPlayers attrs rest (done <> [(p, cards)])
      else chooseOrRunOneM p do
        questionLabeled "lightningSplinters.discard"
        causedBy attrs
        for_ hand \c -> targeting (CardIdTarget c.id) do
          discardFromHand p c
          push
            $ WithTarget (toTarget attrs)
            $ ScenarioSpecific "lightningSplinters.discard"
            $ toJSON ((p, n - 1, cards <> [c]) : rest, done)
