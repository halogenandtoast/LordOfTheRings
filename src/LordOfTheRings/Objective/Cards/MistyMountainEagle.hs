module LordOfTheRings.Objective.Cards.MistyMountainEagle (mistyMountainEagle) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype MistyMountainEagle = MistyMountainEagle ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally in the staging area; once the first player takes control of it, it
-- is an ally (see 'LordOfTheRings.Ally.Cards.MistyMountainEagle').
mistyMountainEagle :: ObjectiveCard MistyMountainEagle
mistyMountainEagle = objective MistyMountainEagle Cards.mistyMountainEagle

-- | Action: The first player may spend 2 Baggins resources to (choose one): take control of
-- Misty Mountain Eagle, or discard this ally to discard a non-unique enemy in the staging
-- area.
instance HasAbilities MistyMountainEagle where
  getAbilities (MistyMountainEagle a) =
    [ anyPlayer $ restricted a 1 (ResourcesOfSphere (ActivePlayer <> FirstPlayer) Baggins 2) $ ActionAbility Free
    | a.placement == StagingArea
    ]

-- | Shadow: Deal 3 damage to attacking enemy.
instance RunMessage MistyMountainEagle where
  runMessage msg o@(MistyMountainEagle attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      spendResources pid Baggins 2
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy)
      chooseOneM pid do
        questionLabeled "mistyMountainEagle.choose"
        causedBy attrs
        labeled "mistyMountainEagle.takeControl" $ putObjectiveAllyIntoPlay attrs.id pid
        targets enemies \e -> do
          discard attrs
          discard e
      pure o
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dealDamage (CardIdSource cid) eid 3
      pure o
    _ -> MistyMountainEagle <$> runMessage msg attrs
