module LordOfTheRings.Treachery.Cards.DrawnToTheRing (drawnToTheRing) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.Fellowship (getReadySagaOneRing)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrawnToTheRing = DrawnToTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drawnToTheRing :: TreacheryCard DrawnToTheRing
drawnToTheRing = treachery DrawnToTheRing Cards.drawnToTheRing

{- | When Revealed: The first player must choose: either exhaust The One Ring and reveal an
encounter card, or discard cards from the top of the encounter deck until an enemy is
discarded and put it into play engaged with him. With The One Ring exhausted, or out of
play, he cannot choose the first.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage DrawnToTheRing where
  runMessage msg t@(DrawnToTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      ring <- getReadySagaOneRing
      chooseOrRunOneM lead do
        questionLabeled "drawnToTheRing.choose"
        causedBy attrs
        for_ ring \oid -> labeled "drawnToTheRing.exhaustTheOneRing" do
          exhaust (ObjectiveTarget oid)
          revealEncounterCard
        labeled "drawnToTheRing.discardUntilEnemy" $ discardEncounterCardsUntil attrs (CardWithType EnemyType)
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      lead <- getLead
      for_ (find (cardMatches $ CardWithType EnemyType) cards) (putEnemyIntoPlayEngagedFromDiscard lead)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachmentBy (CardIdTarget cid) (CardIdSource cid) pid
      pure t
    _ -> DrawnToTheRing <$> runMessage msg attrs
