module LordOfTheRings.Hero.Cards.GrimbeornTheOld (grimbeornTheOld) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype GrimbeornTheOld = GrimbeornTheOld HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimbeornTheOld :: HeroCard GrimbeornTheOld
grimbeornTheOld = hero GrimbeornTheOld Cards.grimbeornTheOld

-- | Response: After Grimbeorn the Old defends an attack, spend 1 resource from his pool to
-- declare him as an attacker against that enemy (and resolve his attack). The defending
-- enemy gets -2 defense for this attack.
--
-- Declared by the card's effect, he attacks whether or not he is ready, as Quick Strike's
-- exhausted character "is immediately declared as an attacker" (FAQ 1.9).
instance HasAbilities GrimbeornTheOld where
  getAbilities (GrimbeornTheOld a) = [mkAbility a 1 $ ResponseAbility (defendedAttack a) (ResourceCost (toTarget a) 1)]

instance RunMessage GrimbeornTheOld where
  runMessage msg h@(GrimbeornTheOld attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (DefendedAttack t eid) <- windows, t == toTarget attrs]) \eid ->
        whenM (selectAny $ EnemyWithId eid) do
          attackModifier attrs eid (DefenseModifier (-2))
          attackEnemyWith pid attrs (toTarget attrs) eid 0
      pure h
    _ -> GrimbeornTheOld <$> runMessage msg attrs
