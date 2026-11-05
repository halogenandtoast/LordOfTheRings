module LordOfTheRings.Ally.Cards.MistyMountainEagle (mistyMountainEagle) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype MistyMountainEagle = MistyMountainEagle AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once a player has taken control of it.
mistyMountainEagle :: AllyCard MistyMountainEagle
mistyMountainEagle = ally MistyMountainEagle Cards.mistyMountainEagle

-- | Action: The first player may spend 2 Baggins resources to (choose one): take control of
-- Misty Mountain Eagle, or discard this ally to discard a non-unique enemy in the staging
-- area. Taking control is open to him while another player controls it.
instance HasAbilities MistyMountainEagle where
  getAbilities (MistyMountainEagle a) =
    [ anyPlayer
        $ restricted a 1 (ResourcesOfSphere (ActivePlayer <> FirstPlayer) Baggins 2 <> AnyCriteria [othersEagle, exists prey])
        $ ActionAbility Free
    ]
   where
    othersEagle = NotCriterion $ exists (AllyWithId a.id <> AllyControlledBy ActivePlayer)
    prey = EnemyInStagingArea <> not_ UniqueEnemy

instance RunMessage MistyMountainEagle where
  runMessage msg s@(MistyMountainEagle a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      spendResources pid Baggins 2
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy)
      chooseOneM pid do
        questionLabeled "mistyMountainEagle.choose"
        causedBy a
        when (a.placement /= InPlayArea pid) $ labeled "mistyMountainEagle.takeControl" $ takeControl a.id pid
        targets enemies \e -> do
          discard a a
          discard a e
      pure s
    _ -> MistyMountainEagle <$> runMessage msg a
