module LordOfTheRings.Treachery.Cards.BeatsAndEchoes (beatsAndEchoes) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Goblin))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BeatsAndEchoes = BeatsAndEchoes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beatsAndEchoes :: TreacheryCard BeatsAndEchoes
beatsAndEchoes = treachery BeatsAndEchoes Cards.beatsAndEchoes

-- | When Revealed: Reveal the top X cards of the encounter deck, adding all enemies to the
-- staging area. Discard the other revealed cards without resolving them. X is 2 plus the
-- number of players not engaged with a Goblin enemy.
-- As The Fate of Balin's Forced, the cards are discarded and the enemies taken back out.
instance RunMessage BeatsAndEchoes where
  runMessage msg t@(BeatsAndEchoes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      free <- filterM (\p -> selectNone $ EnemyWithTrait Goblin <> EnemyEngagedWith (PlayerWithId p)) players
      discardTopOfEncounterDeck attrs (2 + length free)
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard [c | c <- cards, cdCardType (toCardDef c) == EnemyType]
      pure t
    _ -> BeatsAndEchoes <$> runMessage msg attrs
