module LordOfTheRings.Enemy.Cards.AggressiveRacer (aggressiveRacer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (challengersAhead, placeWainridersDamage)

newtype AggressiveRacer = AggressiveRacer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

aggressiveRacer :: EnemyCard AggressiveRacer
aggressiveRacer = enemy AggressiveRacer Cards.aggressiveRacer

-- | Archery 1.
-- While The Challengers is ahead of The Wainriders, Aggressive Racer gains surge.
instance HasModifiersFor AggressiveRacer where
  getModifiersFor (AggressiveRacer a) = whenM challengersAhead $ modifySelf a [GainKeyword Surge]

-- | Forced: After Aggressive Racer attacks and destroys a character, place 2 damage on The
-- Wainriders' stage.
instance HasAbilities AggressiveRacer where
  getAbilities (AggressiveRacer a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage AggressiveRacer where
  runMessage msg e@(AggressiveRacer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeWainridersDamage 2
      pure e
    _ -> AggressiveRacer <$> runMessage msg attrs
