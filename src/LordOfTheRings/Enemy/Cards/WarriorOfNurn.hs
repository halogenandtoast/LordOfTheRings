module LordOfTheRings.Enemy.Cards.WarriorOfNurn (warriorOfNurn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Captain))

newtype WarriorOfNurn = WarriorOfNurn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

warriorOfNurn :: EnemyCard WarriorOfNurn
warriorOfNurn = enemy WarriorOfNurn Cards.warriorOfNurn

-- | Each Captain enemy gets +1 defense.
instance HasModifiersFor WarriorOfNurn where
  getModifiersFor (WarriorOfNurn a) = do
    captains <- select (EnemyWithTrait Captain)
    for_ captains \c -> modified a c [DefenseModifier 1]

-- | Forced: At the end of the round, heal all damage from each Captain enemy in play.
instance HasAbilities WarriorOfNurn where
  getAbilities (WarriorOfNurn a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Shadow: Heal all damage from each Captain enemy in play.
instance RunMessage WarriorOfNurn where
  runMessage msg e@(WarriorOfNurn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      healCaptains
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      healCaptains
      pure e
    _ -> WarriorOfNurn <$> runMessage msg attrs
   where
    healCaptains = traverse_ (healAllDamage attrs) =<< select (EnemyWithTrait Captain)
