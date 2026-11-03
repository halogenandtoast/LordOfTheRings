module LordOfTheRings.Enemy.Cards.SnowGiant (snowGiant) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SnowGiant = SnowGiant EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowGiant :: EnemyCard SnowGiant
snowGiant = enemy SnowGiant Cards.snowGiant

-- | While Snow-Giant is in the staging area, it attacks the first player during the
-- combat phase, as if it were engaged.
instance HasModifiersFor SnowGiant where
  getModifiersFor (SnowGiant a) = when (a.placement == StagingArea) $ modifySelf a [EngagedWithEachPlayer FirstPlayer]

-- | Forced: After Snow-Giant engages you, discard the ally you control with the highest
-- [Willpower].
instance HasAbilities SnowGiant where
  getAbilities (SnowGiant a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage SnowGiant where
  runMessage msg e@(SnowGiant attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        allies <- traverse (\x -> (x,) <$> getWillpower x) . map toTarget =<< select (AllyControlledBy $ PlayerWithId pid)
        let most = maximumMay (map snd allies)
        chooseOrRunOneM pid do
          questionLabeled "snowGiant.discard"
          causedBy attrs
          targets [x | (x, w) <- allies, Just w == most] discard
      pure e
    _ -> SnowGiant <$> runMessage msg attrs
