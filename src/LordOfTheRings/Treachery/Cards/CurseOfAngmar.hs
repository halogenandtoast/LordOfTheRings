module LordOfTheRings.Treachery.Cards.CurseOfAngmar (curseOfAngmar) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (cardTitle)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CurseOfAngmar = CurseOfAngmar TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

curseOfAngmar :: TreacheryCard CurseOfAngmar
curseOfAngmar = treachery CurseOfAngmar Cards.curseOfAngmar

-- | Doomed 1. When Revealed: Each player discards each card in his hand that has the same
-- title as a card in his discard pile. If no cards are discarded by this effect, Curse of
-- Angmar gains surge.
instance RunMessage CurseOfAngmar where
  runMessage msg t@(CurseOfAngmar attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \p -> do
        player <- getPlayer p
        let discardTitles = map (cardTitle . toCardDef) player.discard
            cursed = filter ((`elem` discardTitles) . cardTitle . toCardDef) player.hand
        traverse_ (discardFromHand p) cursed
        pure $ length cursed
      when (sum discarded == 0) revealEncounterCard
      pure t
    _ -> CurseOfAngmar <$> runMessage msg attrs
