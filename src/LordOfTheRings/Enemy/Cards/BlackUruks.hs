module LordOfTheRings.Enemy.Cards.BlackUruks (blackUruks) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BlackUruks = BlackUruks EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackUruks :: EnemyCard BlackUruks
blackUruks = enemy BlackUruks Cards.blackUruks

-- | When Revealed: The first player must choose and discard an attachment from a questing
-- character, if able.
-- Shadow: If this attack is undefended, deal 2 additional shadow cards to attacking enemy.
instance RunMessage BlackUruks where
  runMessage msg e@(BlackUruks attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      heroes <- map toTarget <$> select CommittedHero
      allies <- map toTarget <$> select CommittedAlly
      chooseAndDiscard attrs lead . concat =<< traverse getAttachmentsOn (heroes <> allies)
      pure e
    ResolveShadowEffect cid eid pid [] | cid == attrs.cardId -> do
      dealShadowCard eid
      dealShadowCard eid
      -- the new resolution turns over every shadow card still face down, so the rest of
      -- this one's would resolve twice
      cancelMatching \case
        RevealShadowCard eid' _ -> eid' == eid
        ResolveShadowCard _ eid' _ _ -> eid' == eid
        CheckWindows (Window When (ShadowEffectTriggered _ eid') : _) -> eid' == eid
        _ -> False
      resolveShadowCards eid pid []
      pure e
    _ -> BlackUruks <$> runMessage msg attrs
