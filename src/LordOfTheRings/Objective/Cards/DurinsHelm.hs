module LordOfTheRings.Objective.Cards.DurinsHelm (durinsHelm) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DurinsHelm = DurinsHelm ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

durinsHelm :: ObjectiveCard DurinsHelm
durinsHelm = objective DurinsHelm Cards.durinsHelm

-- | Action: Exhaust a hero to claim this objective. Then, attach Durin's Helm to that hero
-- as an attachment.
instance HasAbilities DurinsHelm where
  getAbilities (DurinsHelm a) =
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

-- | Attached hero gets +1 defense. If attached hero is a Dwarf, it gets +2 hit points.
instance HasModifiersFor DurinsHelm where
  getModifiersFor (DurinsHelm a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      dwarf <- selectAny (HeroWithId hid <> HeroWithTrait Dwarf)
      modified a host $ DefenseModifier 1 : [HitPointModifier 2 | dwarf]
    _ -> pure ()

instance RunMessage DurinsHelm where
  runMessage msg o@(DurinsHelm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      -- the hero exhausted to pay
      for_ [h | Window _ (Exhausted h) <- windows] $ attachObjective attrs.id
      pure o
    _ -> DurinsHelm <$> runMessage msg attrs
