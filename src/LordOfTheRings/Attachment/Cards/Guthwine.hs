module LordOfTheRings.Attachment.Cards.Guthwine (guthwine) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (AttackDestroyed (..), gameAttackDestroyed, gameEffects)
import LordOfTheRings.Trait (Trait (Rohan))

newtype Guthwine = Guthwine AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Rohan hero. Restricted.
guthwine :: AttachmentCard Guthwine
guthwine = attachment Guthwine Cards.guthwine & attachTo (AttachToHero $ HeroWithTrait Rohan)

-- | Response: After attached hero is declared as an attacker, exhaust Gúthwinë to give that
-- hero +2 attack for this attack. If this attack destroys an enemy, return a Rohan ally
-- from your discard pile to your hand.
instance HasAbilities Guthwine where
  getAbilities (Guthwine a) =
    [ mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just host)) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Guthwine where
  runMessage msg s@(Guthwine a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> attackModifier a host (AttackModifier 2)
      pure s
    -- the bonus lasts for this attack: it is still there as the attack resolves, before
    -- the game ends what lasted for it, and the attack's damage has destroyed the enemy
    -- by then
    AttackResolved eid _ attackers _ | AttachedTo host <- a.placement, host `elem` attackers -> do
      boosted <- getsGame (any (\e -> e.effectSource == toSource a && e.effectTarget == host && e.effectDuration == UntilEndOfAttack) . gameEffects)
      destroyed <- getsGame (any (\d -> d.attackDestroyedTarget == EnemyTarget eid && isPlayerAttack d.attackDestroyedBy) . gameAttackDestroyed)
      when (boosted && destroyed) do
        let pid = a.controller
        discards <- (.discard) <$> getPlayer pid
        let allies = [pc | c@(PlayerCard pc) <- discards, cardMatches (CardWithType AllyType <> CardWithTrait Rohan) c]
        chooseOrRunOneM pid do
          questionLabeled "guthwine.chooseAlly"
          causedBy a
          targets allies \c -> returnFromDiscardToHand pid c.id
      Guthwine <$> runMessage msg a
    _ -> Guthwine <$> runMessage msg a
   where
    isPlayerAttack = \case
      PlayerAttackSource _ -> True
      _ -> False
