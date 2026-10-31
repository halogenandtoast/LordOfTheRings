module LordOfTheRings.Enemy.Cards.Graxar (graxar) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Graxar = Graxar EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign.
graxar :: EnemyCard Graxar
graxar = enemy Graxar Cards.graxar

-- | Immune to player card effects. No more than 1 character can attack Graxar each round.
-- Graxar gets +1 attack for each damage on him and +1 defense for each resource on him.
-- ponytail: the engine has no limit on attackers; more than one may still attack Graxar.
instance HasModifiersFor Graxar where
  getModifiersFor (Graxar a) =
    modifySelf a $ ImmuneToPlayerCardEffects : [AttackModifier a.damage | a.damage > 0] <> [DefenseModifier r | let r = countTokens Resource (enemyTokens a), r > 0]

-- | Forced: When Graxar is dealt a shadow card, place 1 resource on him.
instance RunMessage Graxar where
  runMessage msg (Graxar attrs) = case msg of
    CheckWindows ws -> do
      when (or [eid == attrs.id | Window After (ShadowCardDealt eid _ _) <- ws]) $ placeResources attrs attrs 1
      Graxar <$> runMessage msg attrs
    _ -> Graxar <$> runMessage msg attrs
