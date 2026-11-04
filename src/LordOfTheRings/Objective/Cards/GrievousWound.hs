module LordOfTheRings.Objective.Cards.GrievousWound (grievousWound) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Condition))

newtype GrievousWound = GrievousWound ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of Journey in the Dark's burden set. Setup: Add Grievous Wound to the staging
area (the campaign card, or the scenario runner for the Campaign Pool's copy, does).
-}
grievousWound :: ObjectiveCard GrievousWound
grievousWound = objective GrievousWound Cards.grievousWound

-- | Attached, it counts as a Condition attachment.
instance HasModifiersFor GrievousWound where
  getModifiersFor (GrievousWound a) = case a.placement of
    AttachedTo _ -> modifySelf a [GainTrait Condition]
    _ -> pure ()

{- | Forced: After a hero takes damage, if Grievous Wound is unattached, attach to that hero.
(Counts as a Condition attachment with the text: "Forced: After attached hero exhausts,
deal 1 damage to it.")

With several heroes damaged at once, the first player chooses which of them it goes to.
-}
instance HasAbilities GrievousWound where
  getAbilities (GrievousWound a) = case a.placement of
    StagingArea -> [mkAbility a 1 $ forced $ HeroDealtDamageWindow After]
    AttachedTo host@(HeroTarget _) -> [mkAbility a 2 $ forced $ ExhaustedWindow After host]
    _ -> []

instance RunMessage GrievousWound where
  runMessage msg o@(GrievousWound attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      lead <- getLead
      let damaged = ordNub [h | Window _ (DealtDamage (HeroTarget h) _) <- ws]
      chooseOrRunOneM lead do
        questionLabeled "grievousWound.attach"
        causedBy attrs
        targets damaged $ attachObjective attrs.id
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      case attrs.placement of
        AttachedTo host -> dealDamage attrs host 1
        _ -> pure ()
      pure o
    _ -> GrievousWound <$> runMessage msg attrs
