module LordOfTheRings.Treachery.Cards.BlastOfSorcery (blastOfSorcery) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlastOfSorcery = BlastOfSorcery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blastOfSorcery :: TreacheryCard BlastOfSorcery
blastOfSorcery = treachery BlastOfSorcery Cards.blastOfSorcery

{- | When Revealed: Assign X damage among characters committed to the quest. X is 1 more
than the number of players in the game. No player is named, so the first player assigns
it.
Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player has 1 or
fewer cards in his hand).
-}
instance RunMessage BlastOfSorcery where
  runMessage msg t@(BlastOfSorcery attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      players <- getRemainingPlayers
      lead <- getLead
      when (notNull committed) $ dealDamageDividedAmong attrs lead committed (length players + 1)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      hand <- (.hand) <$> getPlayer pid
      enemyGetsAttack (CardIdSource cid) eid (if length hand <= 1 then 2 else 1)
      pure t
    _ -> BlastOfSorcery <$> runMessage msg attrs
