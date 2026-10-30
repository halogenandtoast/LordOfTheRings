module LordOfTheRings.Treachery.Cards.LashingTentacles (lashingTentacles) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LashingTentacles = LashingTentacles TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lashingTentacles :: TreacheryCard LashingTentacles
lashingTentacles = treachery LashingTentacles Cards.lashingTentacles

theThing :: EnemyMatcher
theThing = EnemyWithCardCode (toCardCode Enemies.theThingInTheDepths)

-- | When Revealed: The Thing in the Depths attacks each player in turn order from the
-- staging area (deal and resolve a shadow card for each attack).
-- Shadow: After this attack, The Thing in the Depths makes an attack against the defending
-- player (deal a shadow card for this attack).
instance RunMessage LashingTentacles where
  runMessage msg t@(LashingTentacles attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      things <- select theThing
      for_ things \thing -> eachPlayer \pid -> immediateAttack pid thing
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      things <- select theThing
      for_ things \thing -> afterAttack eid =<< capture (immediateAttack pid thing)
      pure t
    _ -> LashingTentacles <$> runMessage msg attrs
