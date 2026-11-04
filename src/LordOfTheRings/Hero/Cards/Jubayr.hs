module LordOfTheRings.Hero.Cards.Jubayr (jubayr) where

import LordOfTheRings.Enemy.Types (EnemyAttrs (..))
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (eliminatedIfLeavesPlay)

newtype Jubayr = Jubayr HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Objective-hero. Sentinel.
jubayr :: HeroCard Jubayr
jubayr = hero Jubayr Cards.jubayr

-- | Response: After Jubayr is declared as a defender, discard 1 facedown shadow card from
-- a non-unique enemy. (Limit once per phase.)
instance HasAbilities Jubayr where
  getAbilities (Jubayr a) =
    [limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ response $ DefenderDeclaredWindow After (Just $ toTarget a)]

-- | If Jubayr leaves play, you are eliminated from the game.
-- Facedown, an enemy's shadow cards look alike to the player: he picks the enemy, and its
-- first facedown shadow card is discarded.
instance RunMessage Jubayr where
  runMessage msg h@(Jubayr attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- traverse (fmap toAttrs . getEnemy) =<< select (not_ UniqueEnemy)
      let facedown e = [c | c <- enemyShadowCards e, c.id `notElem` (enemyShadowRevealed e <> enemyShadowFaceUp e)]
          shadowed = [(enemyId e, c) | e <- enemies, c : _ <- [facedown e]]
      chooseOrRunOneM pid do
        questionLabeled "jubayr.discard"
        causedBy attrs
        for_ shadowed \(eid, c) -> targeting eid $ discardShadowCard eid c
      pure h
    _ -> do
      eliminatedIfLeavesPlay attrs msg
      Jubayr <$> runMessage msg attrs
