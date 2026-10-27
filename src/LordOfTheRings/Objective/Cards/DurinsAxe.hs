module LordOfTheRings.Objective.Cards.DurinsAxe (durinsAxe) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DurinsAxe = DurinsAxe ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

durinsAxe :: ObjectiveCard DurinsAxe
durinsAxe = objective DurinsAxe Cards.durinsAxe

-- | Action: Exhaust a hero to claim this objective. Then, attach Durin's Axe to that hero
-- as an attachment.
instance HasAbilities DurinsAxe where
  getAbilities (DurinsAxe a) =
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

-- | Attached hero gets +3 attack. If attached hero is a Dwarf, it gets +1 willpower.
instance HasModifiersFor DurinsAxe where
  getModifiersFor (DurinsAxe a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      dwarf <- selectAny (HeroWithId hid <> HeroWithTrait Dwarf)
      modified a host $ AttackModifier 3 : [WillpowerModifier 1 | dwarf]
    _ -> pure ()

instance RunMessage DurinsAxe where
  runMessage msg o@(DurinsAxe attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      -- the hero exhausted to pay
      for_ [h | Window _ (Exhausted h) <- windows] $ attachObjective attrs.id
      pure o
    _ -> DurinsAxe <$> runMessage msg attrs
