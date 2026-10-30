module LordOfTheRings.Hero.Cards.AragornTactics (aragornTactics) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype AragornTactics = AragornTactics HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

aragornTactics :: HeroCard AragornTactics
aragornTactics = hero AragornTactics Cards.aragornTactics

notEngagedWith :: PlayerId -> EnemyMatcher
notEngagedWith = not_ . EnemyEngagedWith . PlayerWithId

-- | Each enemy engaged with you gets -1 defense.
instance HasModifiersFor AragornTactics where
  getModifiersFor (AragornTactics a) = do
    enemies <- select (EnemyEngagedWith $ PlayerWithId a.owner)
    for_ enemies \e -> modified a e [DefenseModifier (-1)]

-- | Response: After Aragorn participates in an attack that destroys an enemy, choose an
-- enemy not engaged with you and engage that enemy.
instance HasAbilities AragornTactics where
  getAbilities (AragornTactics a) =
    [ restricted a 1 (exists $ notEngagedWith a.owner)
        $ response
        $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)
    ]

instance RunMessage AragornTactics where
  runMessage msg h@(AragornTactics attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (notEngagedWith pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemyToEngage"
        causedBy attrs
        targets enemies $ engageEnemy pid
      pure h
    _ -> AragornTactics <$> runMessage msg attrs
