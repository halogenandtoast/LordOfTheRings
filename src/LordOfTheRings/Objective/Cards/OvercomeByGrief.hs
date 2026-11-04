module LordOfTheRings.Objective.Cards.OvercomeByGrief (overcomeByGrief) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Condition))

newtype OvercomeByGrief = OvercomeByGrief ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of Journey in the Dark's burden set. Setup: Add Overcome by Grief to the
staging area (the campaign card, or the scenario runner for the Campaign Pool's copy,
does).
-}
overcomeByGrief :: ObjectiveCard OvercomeByGrief
overcomeByGrief = objective OvercomeByGrief Cards.overcomeByGrief

-- | Attached, it counts as a Condition attachment.
instance HasModifiersFor OvercomeByGrief where
  getModifiersFor (OvercomeByGrief a) = case a.placement of
    AttachedTo _ -> modifySelf a [GainTrait Condition]
    _ -> pure ()

{- | Forced: After a character is destroyed, if Overcome by Grief is unattached, attach to a
hero: the first player chooses which. (Counts as a Condition attachment with the text:
"Forced: After a character you control is destroyed, exhaust attached hero. Until the end
of the round, attached hero cannot ready.")
-}
instance HasAbilities OvercomeByGrief where
  getAbilities (OvercomeByGrief a) = case a.placement of
    StagingArea -> [mkAbility a 1 $ forced $ CharacterDestroyedWindow After]
    AttachedTo host@(HeroTarget _) -> [mkAbility a 2 $ forced $ HostControllersCharacterDestroyedWindow After host]
    _ -> []

instance RunMessage OvercomeByGrief where
  runMessage msg o@(OvercomeByGrief attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      heroes <- select (HeroControlledBy AnyPlayer)
      chooseOrRunOneM lead do
        questionLabeled "overcomeByGrief.attach"
        causedBy attrs
        targets heroes $ attachObjective attrs.id
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      case attrs.placement of
        AttachedTo host -> do
          exhaust host
          roundModifier attrs host CannotReady
        _ -> pure ()
      pure o
    _ -> OvercomeByGrief <$> runMessage msg attrs
