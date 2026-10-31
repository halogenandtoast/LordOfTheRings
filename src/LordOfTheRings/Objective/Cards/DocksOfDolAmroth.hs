module LordOfTheRings.Objective.Cards.DocksOfDolAmroth (docksOfDolAmroth) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype DocksOfDolAmroth = DocksOfDolAmroth ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

docksOfDolAmroth :: ObjectiveCard DocksOfDolAmroth
docksOfDolAmroth = objective DocksOfDolAmroth Cards.docksOfDolAmroth

-- | Guarded. If Docks of Dol Amroth is free of encounters, add it to the staging area,
-- controlled by the players. Action: Exhaust Docks of Dol Amroth to heal 2 damage from a
-- Ship objective. Any player may trigger this action.
instance HasAbilities DocksOfDolAmroth where
  getAbilities (DocksOfDolAmroth a) =
    [ anyPlayer
        $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists shipObjective)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage DocksOfDolAmroth where
  runMessage msg o@(DocksOfDolAmroth attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      ships <- select shipObjective
      chooseOrRunOneM pid do
        questionLabeled "docksOfDolAmroth.heal"
        causedBy attrs
        targets ships \s -> healDamage attrs s 2
      pure o
    _ -> DocksOfDolAmroth <$> runMessage msg attrs
