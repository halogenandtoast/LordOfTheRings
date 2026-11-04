module LordOfTheRings.Enemy.Cards.SauronsEnforcers (sauronsEnforcers) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SauronsEnforcers = SauronsEnforcers EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

sauronsEnforcers :: EnemyCard SauronsEnforcers
sauronsEnforcers = enemy SauronsEnforcers Cards.sauronsEnforcers

-- | X is the sequence number on the main quest.
instance HasModifiersFor SauronsEnforcers where
  getModifiersFor (SauronsEnforcers a) = do
    x <- getMainStage
    modifySelf a [ThreatModifier x, AttackModifier x, DefenseModifier x]

-- | Forced: At the end of the round, assign X damage among characters in play.
instance HasAbilities SauronsEnforcers where
  getAbilities (SauronsEnforcers a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | The first player assigns it, among the characters in play: a hero in the staging area
-- too, which is immune to player card effects only.
instance RunMessage SauronsEnforcers where
  runMessage msg e@(SauronsEnforcers attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- getMainStage
      lead <- getLead
      heroes <- map toTarget <$> select AnyHero
      allies <- map toTarget <$> select AnyAlly
      dealDamageDividedAmong attrs lead (heroes <> allies) x
      pure e
    _ -> SauronsEnforcers <$> runMessage msg attrs
