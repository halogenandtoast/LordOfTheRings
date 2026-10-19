module LordOfTheRings.Enemy.Cards.Wargs (wargs) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Wargs = Wargs EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargs :: EnemyCard Wargs
wargs = enemy Wargs Cards.wargs

-- | Forced: If Wargs is dealt a shadow card with no effect, return Wargs to the staging
-- area after it attacks.
instance HasAbilities Wargs where
  getAbilities (Wargs a) =
    [mkAbility a 1 $ silent $ EnemyAttacksWindow After a.id | any (not . cdShadowEffect . toCardDef) a.enemyShadowCards]

-- | Shadow: attacking enemy gets +1 attack. (+2 attack instead if this attack is undefended.)
instance RunMessage Wargs where
  runMessage msg e@(Wargs attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ PlaceEnemy attrs.id StagingArea
      pure e
    ResolveShadowEffect cid eid _ defender | cid == attrs.cardId -> do
      push $ CreateEffect $ Effect (toSource attrs) (toTarget eid) [AttackModifier $ maybe 2 (const 1) defender] UntilEndOfPhase
      pure e
    _ -> Wargs <$> runMessage msg attrs
