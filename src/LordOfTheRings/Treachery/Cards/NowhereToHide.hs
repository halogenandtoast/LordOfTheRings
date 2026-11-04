module LordOfTheRings.Treachery.Cards.NowhereToHide (nowhereToHide) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NowhereToHide = NowhereToHide TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nowhereToHide :: TreacheryCard NowhereToHide
nowhereToHide = treachery NowhereToHide Cards.nowhereToHide

-- | When Revealed: The first player discards cards from the top of the encounter deck until
-- an enemy is discarded, and puts that enemy into play engaged with him.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if it was engaged this round).
instance RunMessage NowhereToHide where
  runMessage msg t@(NowhereToHide attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardEncounterCardsUntil attrs (CardWithType EnemyType)
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      lead <- getLead
      for_ (find (cardMatches $ CardWithType EnemyType) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged lead card
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      engaged <- elem EngagedThisRound <$> getModifiers eid
      enemyGetsAttack attrs eid (if engaged then 2 else 1)
      pure t
    _ -> NowhereToHide <$> runMessage msg attrs
