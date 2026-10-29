module LordOfTheRings.Objective.Cards.MugashObjective (mugashObjective) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype MugashObjective = MugashObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashObjective :: ObjectiveCard MugashObjective
mugashObjective = objective MugashObjective Cards.mugashObjective

-- | Action: Exhaust a hero to claim this objective when it is free of encounters. Attach
-- Mugash to that hero.
-- Forced: After attached hero takes damage, return Mugash to the top of the encounter deck.
-- Attached to Edge of Fangorn he is guarded by it, and goes to the staging area free as it
-- leaves play.
instance HasAbilities MugashObjective where
  getAbilities (MugashObjective a) = case a.placement of
    StagingArea ->
      [ anyPlayer
          $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists (HeroControlledBy ActivePlayer <> ReadyHero))
          $ ActionAbility Free
      ]
    AttachedTo host@(HeroTarget _) -> [mkAbility a 2 $ forced $ DealtDamageWindow After host]
    _ -> []

instance RunMessage MugashObjective where
  runMessage msg o@(MugashObjective attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOneM pid do
        questionLabeled "mugash.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          attachObjective attrs.id h
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      returnToTopOfEncounterDeck attrs
      pure o
    _ -> MugashObjective <$> runMessage msg attrs
