module LordOfTheRings.Treachery.Cards.ThoseNowSundered (thoseNowSundered) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardRescuedVillagers, objectiveOf)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ThoseNowSundered = ThoseNowSundered TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thoseNowSundered :: TreacheryCard ThoseNowSundered
thoseNowSundered = treachery ThoseNowSundered Cards.thoseNowSundered

-- | Counts as a Condition attachment with the text: "Forced: At the end of the round,
-- discard 1 villager token from Rescued Villagers."
instance HasAbilities ThoseNowSundered where
  getAbilities (ThoseNowSundered a) = case a.placement of
    AttachedTo (ObjectiveTarget _) -> [mkAbility a 1 $ forced RoundEndsWindow]
    _ -> []

-- | Surge. Doomed 1.
-- When Revealed: Attach to Rescued Villagers.
instance RunMessage ThoseNowSundered where
  runMessage msg t@(ThoseNowSundered attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< objectiveOf Objectives.rescuedVillagers
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardRescuedVillagers attrs 1
      pure t
    _ -> ThoseNowSundered <$> runMessage msg attrs
