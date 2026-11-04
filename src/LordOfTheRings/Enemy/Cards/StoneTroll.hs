module LordOfTheRings.Enemy.Cards.StoneTroll (stoneTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype StoneTroll = StoneTroll EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

stoneTroll :: EnemyCard StoneTroll
stoneTroll = enemy StoneTroll Cards.stoneTroll

-- | Cannot have non-objective attachments.
instance HasModifiersFor StoneTroll where
  getModifiersFor (StoneTroll a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Stone-troll attacks and destroys a character you control, discard 1 card
-- from your hand for each excess point of damage dealt. Read as the excess is dealt, the
-- character it went past already destroyed.
instance HasAbilities StoneTroll where
  getAbilities (StoneTroll a) = [mkAbility a 1 $ forced $ EnemyDealtExcessDamageWindow After a.id]

-- | Shadow: Discard an attachment you control.
instance RunMessage StoneTroll where
  runMessage msg e@(StoneTroll attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      destroyed <- attackDestroyed attrs.id
      for_ [(t, n) | Window After (AttackDealtExcessDamage eid t n) <- ws, eid == attrs.id] \(t, n) ->
        for_ [p | d <- destroyed, d.target == t, Just p <- [d.controller]] \p ->
          chooseAndDiscardFromHand p attrs n
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard (CardIdSource cid) pid attachments
      pure e
    _ -> StoneTroll <$> runMessage msg attrs
