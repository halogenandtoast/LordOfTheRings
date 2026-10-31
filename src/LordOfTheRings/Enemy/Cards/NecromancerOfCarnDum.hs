module LordOfTheRings.Enemy.Cards.NecromancerOfCarnDum (necromancerOfCarnDum) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype NecromancerOfCarnDum = NecromancerOfCarnDum EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

necromancerOfCarnDum :: EnemyCard NecromancerOfCarnDum
necromancerOfCarnDum = enemy NecromancerOfCarnDum Cards.necromancerOfCarnDum

-- | Forced: When Necromancer of Carn Dûm would attack you, instead reanimate the top card
-- of your deck and deal it each of the Necromancer of Carn Dûm's shadow cards.
instance HasAbilities NecromancerOfCarnDum where
  getAbilities (NecromancerOfCarnDum a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage NecromancerOfCarnDum where
  runMessage msg e@(NecromancerOfCarnDum attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      cancelAttack attrs.id
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> do
        deck <- (.deck) <$> getPlayer pid
        for_ (take 1 deck) \top -> do
          dead <- reanimate top.id
          for_ attrs.shadowCards \card -> do
            removeShadowCard attrs.id card.id
            whileInPlay (EnemyTarget dead) $ push $ AttachShadowCard dead card
      pure e
    _ -> NecromancerOfCarnDum <$> runMessage msg attrs
