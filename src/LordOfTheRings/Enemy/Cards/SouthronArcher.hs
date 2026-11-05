module LordOfTheRings.Enemy.Cards.SouthronArcher (southronArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SouthronArcher = SouthronArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Archery 2, as its card def says.
southronArcher :: EnemyCard SouthronArcher
southronArcher = enemy SouthronArcher Cards.southronArcher

-- | Forced: After Southron Archer engages you, either remove 2 progress tokens from the
-- main quest, or discard an ally you control. Either is offered only when it can be done
-- in full (FAQ 1.44): with neither, nothing happens.
instance HasAbilities SouthronArcher where
  getAbilities (SouthronArcher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage SouthronArcher where
  runMessage msg e@(SouthronArcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "southronArcher.choose"
          causedBy attrs
          removeMainQuestProgressLabeled attrs 2
          unless (null allies) $ labeled "southronArcher.discardAlly" do
            chooseOrRunOneM pid do
              questionLabeled "southronArcher.ally"
              causedBy attrs
              targets allies (discard attrs)
      pure e
    _ -> SouthronArcher <$> runMessage msg attrs
