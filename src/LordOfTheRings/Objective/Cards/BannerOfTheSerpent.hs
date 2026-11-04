module LordOfTheRings.Objective.Cards.BannerOfTheSerpent (bannerOfTheSerpent) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (attachedToEncounterCard)
import LordOfTheRings.Trait (Trait (Harad))

newtype BannerOfTheSerpent = BannerOfTheSerpent ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

bannerOfTheSerpent :: ObjectiveCard BannerOfTheSerpent
bannerOfTheSerpent = objective BannerOfTheSerpent Cards.bannerOfTheSerpent

-- | Guarded (enemy or location). While Banner of the Serpent is attached to an encounter
-- card, each Harad enemy gets +1 attack.
instance HasModifiersFor BannerOfTheSerpent where
  getModifiersFor (BannerOfTheSerpent a) = whenM (attachedToEncounterCard a) do
    enemies <- select (EnemyWithTrait Harad)
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Action: Exhaust Banner of the Serpent and raise your threat by 1 to give each Harad
-- enemy engaged with you -1 attack until the end of the phase.
instance HasAbilities BannerOfTheSerpent where
  getAbilities (BannerOfTheSerpent a) =
    [mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, RaiseThreatCost 1]) | AttachedTo _ <- [a.placement], not a.exhausted]

instance RunMessage BannerOfTheSerpent where
  runMessage msg o@(BannerOfTheSerpent attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (EnemyWithTrait Harad <> EnemyEngagedWith (PlayerWithId pid))
      for_ enemies \e -> phaseModifier attrs e (AttackModifier (-1))
      pure o
    _ -> BannerOfTheSerpent <$> runMessage msg attrs
