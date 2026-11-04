module LordOfTheRings.Enemy.Cards.BlackBats (blackBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Dark))

newtype BlackBats = BlackBats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackBats :: EnemyCard BlackBats
blackBats = enemy BlackBats Cards.blackBats

-- | Surge. While the active location has the Dark trait, Black Bats cannot take damage.
instance HasModifiersFor BlackBats where
  getModifiersFor (BlackBats a) = do
    dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
    when dark $ modifySelf a [CannotTakeDamage]

-- | Shadow: If the active location has the Dark trait, attacking enemy cannot take damage
-- this phase.
instance RunMessage BlackBats where
  runMessage msg e@(BlackBats attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
      when dark $ phaseModifier (CardIdSource cid) eid CannotTakeDamage
      pure e
    _ -> BlackBats <$> runMessage msg attrs
