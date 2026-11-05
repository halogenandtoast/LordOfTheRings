module LordOfTheRings.Objective.Cards.AshenBrazier (ashenBrazier) where

import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjective)

newtype AshenBrazier = AshenBrazier ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Ered Mithrin campaign, shuffled in by The Ghost of Framsburg's campaign
-- card. Loot.
ashenBrazier :: ObjectiveCard AshenBrazier
ashenBrazier = objective AshenBrazier Cards.ashenBrazier

-- | If unattached, the first player claims Ashen Brazier and attaches it to a hero they
-- control.
instance HasModifiersFor AshenBrazier where
  getModifiersFor (AshenBrazier a) = lootObjective a

watched :: EnemyMatcher
watched = EnemyEngagedWith AnyPlayer <> EnemyWithFacedownShadow

-- | Action: Exhaust Ashen Brazier to look at a facedown shadow card.
instance HasAbilities AshenBrazier where
  getAbilities (AshenBrazier a) =
    [ restricted a 1 (exists watched) $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    , not a.exhausted
    ]

instance RunMessage AshenBrazier where
  runMessage msg o@(AshenBrazier attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select watched
      chooseOrRunOneM pid do
        questionLabeled "ashenBrazier.enemy"
        causedBy attrs
        targets enemies \eid -> do
          enemy <- getEnemy eid
          let facedown = [c | c <- enemy.shadowCards, c.id `notElem` enemyShadowRevealed (toAttrs enemy)]
          chooseOrRunOneM pid do
            questionLabeled "ashenBrazier.shadow"
            causedBy attrs
            targets facedown \card -> lookAtShadowCard pid attrs eid card.id
      pure o
    _ -> AshenBrazier <$> runMessage msg attrs
