module LordOfTheRings.Objective.Cards.Aearlang (aearlang) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Aearlang = Aearlang ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aearlang :: ObjectiveCard Aearlang
aearlang = objective Aearlang Cards.aearlang

-- | Guarded. If Aearlang is free of encounters, add it to the staging area, controlled by
-- the players. Action: Exhaust Aearlang to ready a hero. Any player may trigger this
-- action.
instance HasAbilities Aearlang where
  getAbilities (Aearlang a) =
    [ anyPlayer
        $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists ExhaustedHero)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage Aearlang where
  runMessage msg o@(Aearlang attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select ExhaustedHero
      chooseOrRunOneM pid do
        questionLabeled "aearlang.ready"
        causedBy attrs
        targets heroes ready
      pure o
    _ -> Aearlang <$> runMessage msg attrs
