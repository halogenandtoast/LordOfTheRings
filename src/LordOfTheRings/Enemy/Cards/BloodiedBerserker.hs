module LordOfTheRings.Enemy.Cards.BloodiedBerserker (bloodiedBerserker) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)

newtype BloodiedBerserker = BloodiedBerserker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bloodiedBerserker :: EnemyCard BloodiedBerserker
bloodiedBerserker = enemy BloodiedBerserker Cards.bloodiedBerserker

-- "Dealt a shadow card with ..." is checked as it turns faceup, not while
-- facedown or after a discarded shadow (Nate French, ALeP 3.003; Caleb, 3.274 #3).
instance HasAbilities BloodiedBerserker where
  getAbilities (BloodiedBerserker a) = [mkAbility a 1 $ forced $ ShadowCardWithoutEffectWindow When a.id]

instance RunMessage BloodiedBerserker where
  runMessage msg e@(BloodiedBerserker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | (card, pid, _) <- revealedShadowCards a windows, not (cdShadowEffect $ toCardDef card)] \pid -> do
        active <- filterM (fmap not . immuneToSource (toSource a)) =<< select IsActiveLocation
        mods <- getModifiers a
        chooseOrRunOneM pid do
          questionLabeled "bloodiedBerserker.choose"
          causedBy a
          when (notNull active) $ labeled "bloodiedBerserker.progress" do
            chooseOrRunOneM pid do
              questionLabeled "bloodiedBerserker.location"
              causedBy a
              targets active (\lid -> placeProgress a lid 2)
          when (CannotAttack `notElem` mods && CannotAttackPlayer pid `notElem` mods) $
            labeled "bloodiedBerserker.attack" $ phaseModifier a a (AdditionalAttackAfterNextAttack pid)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      pure e
    _ -> BloodiedBerserker <$> runMessage msg a
