module LordOfTheRings.Hero.Cards.BeregondSpirit (beregondSpirit) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype BeregondSpirit = BeregondSpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel.
beregondSpirit :: HeroCard BeregondSpirit
beregondSpirit = hero BeregondSpirit Cards.beregondSpirit

-- | Response: After Beregond defends an attack and takes no damage, reduce the defending
-- player's threat by 1. (Limit once per round.)
instance HasAbilities BeregondSpirit where
  getAbilities (BeregondSpirit a) =
    [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ response $ DefendedWithoutDamageWindow After (toTarget a)]

instance RunMessage BeregondSpirit where
  runMessage msg h@(BeregondSpirit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      -- the defending player is the one the attacker is engaged with
      for_ (take 1 [eid | Window _ (DefendedWithoutDamage _ eid) <- windows]) \eid -> do
        defending <- placementController . (.placement) <$> getEnemy eid
        for_ defending \p -> lowerThreatBy attrs p 1
      pure h
    _ -> BeregondSpirit <$> runMessage msg attrs
