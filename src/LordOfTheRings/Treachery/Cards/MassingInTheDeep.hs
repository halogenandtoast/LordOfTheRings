module LordOfTheRings.Treachery.Cards.MassingInTheDeep (massingInTheDeep) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MassingInTheDeep = MassingInTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

massingInTheDeep :: TreacheryCard MassingInTheDeep
massingInTheDeep = treachery MassingInTheDeep Cards.massingInTheDeep

-- | Doomed 1. When Revealed: Reveal X additional cards from the encounter deck and add them
-- to the staging area. X is the number of players in the game.
-- Shadow: attacking enemy gets +X attack. X is the number of players in the game.
instance RunMessage MassingInTheDeep where
  runMessage msg t@(MassingInTheDeep attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealEncounterCards . length =<< getRemainingPlayers
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid . length =<< getRemainingPlayers
      pure t
    _ -> MassingInTheDeep <$> runMessage msg attrs
