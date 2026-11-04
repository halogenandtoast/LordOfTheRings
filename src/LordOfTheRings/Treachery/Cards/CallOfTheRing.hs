module LordOfTheRings.Treachery.Cards.CallOfTheRing (callOfTheRing) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CallOfTheRing = CallOfTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

callOfTheRing :: TreacheryCard CallOfTheRing
callOfTheRing = treachery CallOfTheRing Cards.callOfTheRing

nazgulEnemy :: CardMatcher
nazgulEnemy = CardMatchAll [CardWithType EnemyType, CardWithTrait Nazgul]

-- | When Revealed: The first player must choose: either search the encounter deck, discard
-- pile, and victory display for a Nazgûl enemy and add it to the staging area, or exhaust
-- The One Ring and reveal the top card of the encounter deck. Either may be chosen, but one
-- that can be done in full must be (FAQ 1.44).
-- Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage CallOfTheRing where
  runMessage msg t@(CallOfTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      s <- getsGame (toAttrs . (.scenario))
      let nazgul = any (cardMatches nazgulEnemy) (s.encounterDeck <> s.encounterDiscard <> [c | EncounterCard c <- s.victoryDisplay])
      ring <- getSagaOneRing
      exhausted <- theOneRingExhausted
      let readyRing = guard (not exhausted) *> ring
          -- with neither able to be done in full, either is done as far as it can be
          neither = not nazgul && isNothing readyRing
      chooseOrRunOneM lead do
        questionLabeled "callOfTheRing.choose"
        causedBy attrs
        when (nazgul || neither) $ labeled "callOfTheRing.search" $ searchEncounterCardsAndVictoryDisplay nazgulEnemy addToStaging
        when (isJust readyRing || neither) $ labeled "callOfTheRing.exhaust" do
          for_ readyRing (exhaust . toTarget)
          revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> CallOfTheRing <$> runMessage msg attrs
