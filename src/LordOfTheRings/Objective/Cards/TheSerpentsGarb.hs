module LordOfTheRings.Objective.Cards.TheSerpentsGarb (theSerpentsGarb) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (encounterCardOf)
import LordOfTheRings.Trait (Trait (Harad))

newtype TheSerpentsGarb = TheSerpentsGarb ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theSerpentsGarb :: ObjectiveCard TheSerpentsGarb
theSerpentsGarb = objective TheSerpentsGarb Cards.theSerpentsGarb

-- | Guarded (enemy). Attached encounter card gets +2 defense and is immune to player card
-- effects.
instance HasModifiersFor TheSerpentsGarb where
  getModifiersFor (TheSerpentsGarb a) = do
    attached <- encounterCardOf a
    for_ attached \t -> modified a t [DefenseModifier 2, ImmuneToPlayerCardEffects]

-- | Action: Exhaust The Serpent's Garb and raise your threat by 1 to give a Harad enemy in
-- the staging area -2 defense until the end of the phase.
instance HasAbilities TheSerpentsGarb where
  getAbilities (TheSerpentsGarb a) =
    [ restricted a 1 (exists $ EnemyWithTrait Harad <> EnemyInStagingArea) $ ActionAbility (Costs [exhaustCost a, RaiseThreatCost 1])
    | AttachedTo _ <- [a.placement]
    , not a.exhausted
    ]

instance RunMessage TheSerpentsGarb where
  runMessage msg o@(TheSerpentsGarb attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (EnemyWithTrait Harad <> EnemyInStagingArea)
      chooseOrRunOneM pid do
        questionLabeled "theSerpentsGarb.enemy"
        causedBy attrs
        targets enemies \e -> phaseModifier attrs e (DefenseModifier (-2))
      pure o
    _ -> TheSerpentsGarb <$> runMessage msg attrs
