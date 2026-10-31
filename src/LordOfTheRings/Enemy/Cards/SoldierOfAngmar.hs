module LordOfTheRings.Enemy.Cards.SoldierOfAngmar (soldierOfAngmar) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype SoldierOfAngmar = SoldierOfAngmar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

soldierOfAngmar :: EnemyCard SoldierOfAngmar
soldierOfAngmar = enemy SoldierOfAngmar Cards.soldierOfAngmar

-- | Forced: After Soldier of Angmar attacks and destroys an ally, search the encounter
-- deck, discard pile and victory display for Orc Ambush and reveal it. Shuffle the
-- encounter deck.
instance HasAbilities SoldierOfAngmar where
  getAbilities (SoldierOfAngmar a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Attacking enemy gets +2 attack.
instance RunMessage SoldierOfAngmar where
  runMessage msg e@(SoldierOfAngmar attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      unless (null [() | Window _ (CharacterDestroyedByAttack _ (AllyTarget _) _) <- ws])
        $ searchAndRevealEncounterCard (cardIs Quests.orcAmbush)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> SoldierOfAngmar <$> runMessage msg attrs
