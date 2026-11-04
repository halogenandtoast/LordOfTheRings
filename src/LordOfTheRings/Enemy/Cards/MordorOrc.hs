module LordOfTheRings.Enemy.Cards.MordorOrc (mordorOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (getMinasTirith)

newtype MordorOrc = MordorOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
mordorOrc :: EnemyCard MordorOrc
mordorOrc = enemy MordorOrc Cards.mordorOrc

-- | While Minas Tirith is in play, undefended damage dealt by Mordor Orc must be assigned
-- to Minas Tirith.
instance HasModifiersFor MordorOrc where
  getModifiersFor (MordorOrc a) = getMinasTirith >>= traverse_ \o -> modifySelf a [UndefendedDamageTo (ObjectiveTarget o)]

-- | Shadow: If Minas Tirith is in play, engage Mordor Orc and deal it a shadow card. The
-- shadow card becomes an enemy engaged with the defending player, found by its card.
instance RunMessage MordorOrc where
  runMessage msg e@(MordorOrc attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      inPlay <- isJust <$> getMinasTirith
      when inPlay do
        putShadowEnemyIntoPlay eid cid pid
        doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      select (EnemyWithCardId cid) >>= traverse_ dealShadowCard
      pure e
    _ -> MordorOrc <$> runMessage msg attrs
