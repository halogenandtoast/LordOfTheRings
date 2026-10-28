module LordOfTheRings.Objective.Cards.AlcaronsScroll (alcaronsScroll) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype AlcaronsScroll = AlcaronsScroll ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

alcaronsScroll :: ObjectiveCard AlcaronsScroll
alcaronsScroll = objective AlcaronsScroll Cards.alcaronsScroll

-- | If unattached, return Alcaron's Scroll to the staging area and it gains: "Action:
-- Exhaust a hero to attach Alcaron's Scroll to that hero."
instance HasAbilities AlcaronsScroll where
  getAbilities (AlcaronsScroll a) =
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

-- | Forced: When the hero with Alcaron's Scroll attached is damaged by an enemy attack,
-- attach Alcaron's Scroll to that enemy.
-- ponytail: read off the damage itself as it is dealt, an enemy's damage being its attack's;
-- no window says which enemy dealt it.
instance RunMessage AlcaronsScroll where
  runMessage msg o@(AlcaronsScroll attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [h | Window _ (Exhausted h) <- windows] $ attachObjective attrs.id
      pure o
    DoStep 1 (DealDamage (EnemySource eid) host@(HeroTarget _) _) | attrs.placement == AttachedTo host -> do
      attachObjective attrs.id (EnemyTarget eid)
      pure o
    _ -> AlcaronsScroll <$> runMessage msg attrs
