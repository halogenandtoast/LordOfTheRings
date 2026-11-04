module LordOfTheRings.Enemy.Cards.UrukWargRider (urukWargRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)

newtype UrukWargRider = UrukWargRider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukWargRider :: EnemyCard UrukWargRider
urukWargRider = enemy UrukWargRider Cards.urukWargRider

-- | Uruk Warg-rider gets +X attack while attacking a character with less willpower than
-- Uruk Warg-rider's threat. X is the difference. It attacks the characters defending its
-- attack; undefended, it attacks no character. Against several, X is the greatest.
instance HasModifiersFor UrukWargRider where
  getModifiersFor (UrukWargRider a) = do
    defenders <- getDefendersAgainst a.id
    unless (null defenders) do
      threat <- getEnemyThreat a.id
      x <- foldr max 0 <$> traverse (fmap (threat -) . getWillpower) defenders
      when (x > 0) $ modifySelf a [AttackModifier x]

-- | Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage UrukWargRider where
  runMessage msg e@(UrukWargRider attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> UrukWargRider <$> runMessage msg attrs
