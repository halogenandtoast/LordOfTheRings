module LordOfTheRings.Enemy.Cards.SoldierOfIsengard (soldierOfIsengard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SoldierOfIsengard = SoldierOfIsengard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Toughness 1. Archery 1, as its card def says.
soldierOfIsengard :: EnemyCard SoldierOfIsengard
soldierOfIsengard = enemy SoldierOfIsengard Cards.soldierOfIsengard

-- | Forced: After Soldier of Isengard attacks and destroys a character, place 1 progress
-- on the current quest (the active location taking it first, FAQ 2.02).
instance HasAbilities SoldierOfIsengard where
  getAbilities (SoldierOfIsengard a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Exhaust a character you control.
instance RunMessage SoldierOfIsengard where
  runMessage msg e@(SoldierOfIsengard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgressOnQuest attrs 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust (CardIdSource cid) pid 1
      pure e
    _ -> SoldierOfIsengard <$> runMessage msg attrs
