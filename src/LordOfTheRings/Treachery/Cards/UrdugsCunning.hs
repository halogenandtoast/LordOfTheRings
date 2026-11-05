module LordOfTheRings.Treachery.Cards.UrdugsCunning (urdugsCunning) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UrdugsCunning = UrdugsCunning TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, from Roam Across Rhovanion. Doomed 1. Surge.
urdugsCunning :: TreacheryCard UrdugsCunning
urdugsCunning = treachery UrdugsCunning Cards.urdugsCunning

{- | When Revealed: Discard the top 5 cards of the encounter deck. For each enemy discarded
this way, each player must deal 1 damage to a character they control.
Shadow: Attacking enemy gets +2 [defense] this phase.
-}
instance RunMessage UrdugsCunning where
  runMessage msg t@(UrdugsCunning attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardTopOfEncounterDeck attrs 5
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let enemies = length (filter (cardMatches $ CardWithType EnemyType) cards)
      when (enemies > 0) $ eachPlayer \p -> replicateM_ enemies (dealDamageToOneCharacter attrs p 1)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 2)
      pure t
    _ -> UrdugsCunning <$> runMessage msg attrs
