module LordOfTheRings.Enemy.Cards.UrukElfStalker (urukElfStalker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoadToRivendell.Helpers (getArwen)

newtype UrukElfStalker = UrukElfStalker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukElfStalker :: EnemyCard UrukElfStalker
urukElfStalker = enemy UrukElfStalker Cards.urukElfStalker

-- | Forced: When Uruk Elf-stalker attacks and destroys a character, deal 1 damage to Arwen
-- Undómiel.
instance HasAbilities UrukElfStalker where
  getAbilities (UrukElfStalker a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Attacking enemy gets +2 [Attack].
instance RunMessage UrukElfStalker where
  runMessage msg e@(UrukElfStalker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      arwen <- getArwen
      for_ arwen \a -> dealDamage attrs a 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> UrukElfStalker <$> runMessage msg attrs
