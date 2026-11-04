module LordOfTheRings.Enemy.Cards.GollumDungeonsDeep (gollumDungeonsDeep) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GollumDungeonsDeep = GollumDungeonsDeep EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumDungeonsDeep :: EnemyCard GollumDungeonsDeep
gollumDungeonsDeep = enemy GollumDungeonsDeep Cards.gollumDungeonsDeep

{- | Forced: After the first player answers a riddle and fails to find at least 1 match,
Gollum attacks Bilbo Baggins. (Do not deal a shadow card for this attack.)

An attack against a character: any character of Bilbo's controller may defend, and
undefended its damage must go to Bilbo (FAQ 1.41).
-}
instance RunMessage GollumDungeonsDeep where
  runMessage msg e@(GollumDungeonsDeep attrs) = case msg of
    ScenarioSpecific "riddle.answered" (parseMaybe parseJSON -> Just (_ :: Source, 0 :: Int)) -> do
      getBilbo >>= traverse_ \b -> do
        bilbo <- getHero b
        attackModifier attrs attrs.id (UndefendedDamageTo $ toTarget b)
        immediateAttackWithoutShadow bilbo.owner attrs.id
      pure e
    _ -> GollumDungeonsDeep <$> runMessage msg attrs
