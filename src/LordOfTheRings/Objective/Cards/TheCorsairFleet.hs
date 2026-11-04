module LordOfTheRings.Objective.Cards.TheCorsairFleet (theCorsairFleet) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TheCorsairFleet = TheCorsairFleet ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its Corsair side, as The Siege of Gondor plays it.
theCorsairFleet :: ObjectiveCard TheCorsairFleet
theCorsairFleet = objective TheCorsairFleet Cards.theCorsairFleet

-- | Forced: At the beginning of the staging step, place 1 resource on this objective. Then,
-- each player must choose: either reveal an encounter card, or assign X damage among
-- characters he controls. X is the number of resources here.
instance HasAbilities TheCorsairFleet where
  getAbilities (TheCorsairFleet a) = [mkAbility a 1 $ forced StagingStepBeginsWindow]

-- | X is read once the resource is placed. A player revealing a card is its "you".
instance RunMessage TheCorsairFleet where
  runMessage msg o@(TheCorsairFleet attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      doStep 2 msg
      pure o
    DoStep 2 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeledWith "theCorsairFleet.choose" [intVar "n" attrs.resources]
        causedBy attrs
        labeled "theCorsairFleet.reveal" $ revealEncounterCardAs p
        labeledWith "theCorsairFleet.damage" [intVar "n" attrs.resources] $ dealDamageDivided attrs p attrs.resources
      pure o
    _ -> TheCorsairFleet <$> runMessage msg attrs
