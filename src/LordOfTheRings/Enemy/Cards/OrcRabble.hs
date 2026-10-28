module LordOfTheRings.Enemy.Cards.OrcRabble (orcRabble) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcRabble = OrcRabble EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcRabble :: EnemyCard OrcRabble
orcRabble = enemy OrcRabble Cards.orcRabble

-- | Forced: When Orc Rabble is dealt a shadow card, it gets +2 attack until the end of the
-- phase.
-- Shadow: Deal the attacking enemy an additional shadow card for each player in the game.
instance RunMessage OrcRabble where
  runMessage msg e@(OrcRabble attrs) = case msg of
    AttachShadowCard eid _ | eid == attrs.id -> do
      phaseModifier attrs attrs (AttackModifier 2)
      OrcRabble <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      players <- length <$> getRemainingPlayers
      dealShadowCardsDuringAttack eid pid defenders players
      pure e
    _ -> OrcRabble <$> runMessage msg attrs
