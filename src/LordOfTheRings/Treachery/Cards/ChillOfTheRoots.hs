module LordOfTheRings.Treachery.Cards.ChillOfTheRoots (chillOfTheRoots) where

import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (attachFromTopOfDeck)
import LordOfTheRings.Trait (Trait (Nameless))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ChillOfTheRoots = ChillOfTheRoots TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chillOfTheRoots :: TreacheryCard ChillOfTheRoots
chillOfTheRoots = treachery ChillOfTheRoots Cards.chillOfTheRoots

-- | When Revealed: Each player must either discard his hand, or raise his threat by the
-- number of cards in his hand. If no cards were discarded by this effect, Chill of the
-- Roots gains surge.
-- Shadow: If attacking enemy is Nameless, attach the top card of the defending player's
-- deck to it.
--
-- A hand discarded marks the card with a resource token while it resolves, so the surge
-- check after the last player knows.
instance RunMessage ChillOfTheRoots where
  runMessage msg t@(ChillOfTheRoots attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        hand <- discardableHand <$> getPlayer p
        chooseOneM p do
          questionLabeled "chillOfTheRoots.choose"
          causedBy attrs
          labeled "chillOfTheRoots.discardHand" do
            discardHand p
            when (notNull hand) $ placeResources attrs attrs 1
          labeled "chillOfTheRoots.raiseThreat" $ encounterRaisesThreat p (length hand)
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      when (countTokens Resource (treacheryTokens attrs) == 0) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      nameless <- selectAny (EnemyWithId eid <> EnemyWithTrait Nameless)
      when nameless $ attachFromTopOfDeck pid 1 (EnemyTarget eid)
      pure t
    _ -> ChillOfTheRoots <$> runMessage msg attrs
