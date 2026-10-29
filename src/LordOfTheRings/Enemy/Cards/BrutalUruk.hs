module LordOfTheRings.Enemy.Cards.BrutalUruk (brutalUruk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (turnHiddenCardsFaceup)

newtype BrutalUruk = BrutalUruk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brutalUruk :: EnemyCard BrutalUruk
brutalUruk = enemy BrutalUruk Cards.brutalUruk

-- | Forced: After Brutal Uruk attacks and destroys a character, that character's controller
-- turns all hidden cards in his play area faceup. Deal a shadow card to each enemy turned
-- faceup by this effect and resolve their attacks as normal.
instance HasAbilities BrutalUruk where
  getAbilities (BrutalUruk a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | The enemies turned faceup attack once Brutal Uruk's attack is over, so the hidden cards
-- wait for it too.
instance RunMessage BrutalUruk where
  runMessage msg e@(BrutalUruk attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws] \pid ->
        afterAttack attrs.id [DoStep 1 (UseCardAbility pid (toSource attrs) 1 [])]
      pure e
    DoStep 1 (UseCardAbility pid (isSource attrs -> True) 1 _) -> do
      enemies <- turnHiddenCardsFaceup pid
      for_ enemies \enemy' -> whileInPlay enemy' $ dealShadowCard enemy'
      for_ enemies \enemy' -> whileInPlay enemy' $ push $ EnemyAttack pid enemy'
      pure e
    _ -> BrutalUruk <$> runMessage msg attrs
