module LordOfTheRings.Treachery.Cards.MugashsCunning (mugashsCunning) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (cdCardType), CardType (EnemyType), toCardDef)
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (discardTopOfOutOfPlayDeck, outOfPlayDeckOf)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MugashsCunning = MugashsCunning TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashsCunning :: TreacheryCard MugashsCunning
mugashsCunning = treachery MugashsCunning Cards.mugashsCunning

-- | When Revealed: Each player discards the top 3 cards of his out-of-play deck. Add each
-- enemy discarded this way to the staging area. If no enemies are added to the staging
-- area this way, Mugash's Cunning gains surge.
-- Shadow: Attacking enemy gets +3 Attack and +3 Defense until the end of the round.
instance RunMessage MugashsCunning where
  runMessage msg t@(MugashsCunning attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      tops <- concatMap (take 3) <$> traverse outOfPlayDeckOf players
      for_ players \p -> discardTopOfOutOfPlayDeck p 3
      unless (any isEnemy tops) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifiers attrs eid [AttackModifier 3, DefenseModifier 3]
      pure t
    _ -> MugashsCunning <$> runMessage msg attrs
   where
    isEnemy = \case
      EncounterCard ec -> cdCardType (toCardDef ec) == EnemyType
      _ -> False
