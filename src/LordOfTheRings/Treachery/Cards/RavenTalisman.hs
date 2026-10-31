module LordOfTheRings.Treachery.Cards.RavenTalisman (ravenTalisman) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RavenTalisman = RavenTalisman TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenTalisman :: TreacheryCard RavenTalisman
ravenTalisman = treachery RavenTalisman Cards.ravenTalisman

-- | When Revealed: Each player draws 1 card. Until the end of the round, add 1 to the total
-- threat in the staging area for each card in the first player's hand.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, discard your
-- hand and draw three cards.
instance RunMessage RavenTalisman where
  runMessage msg t@(RavenTalisman attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> drawCards attrs p 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- length . (.hand) <$> (getPlayer =<< getLead)
      roundModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardHand pid
      drawCards attrs pid 3
      pure t
    _ -> RavenTalisman <$> runMessage msg attrs
