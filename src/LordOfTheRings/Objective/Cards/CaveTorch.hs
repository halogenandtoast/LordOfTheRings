module LordOfTheRings.Objective.Cards.CaveTorch (caveTorch) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Dark))

newtype CaveTorch = CaveTorch ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveTorch :: ObjectiveCard CaveTorch
caveTorch = objective CaveTorch Cards.caveTorch

-- | Attach to a hero. Restricted.
-- Action: Exhaust Cave Torch to place up to 3 progress on a Dark location.
-- Forced: After Cave Torch exhausts, discard the top card of the encounter deck. If that
-- card is an enemy, add it to the staging area.
-- The exhaust is paid as the Action resolves, after its progress is placed: its Forced
-- effect follows the progress, and a Twisting Passage's locate test before it (ruling
-- 3.097).
instance HasAbilities CaveTorch where
  getAbilities (CaveTorch a) = case a.placement of
    AttachedTo _ ->
      [restricted a 1 (exists $ LocationWithTrait Dark) $ ActionAbility Free | not a.exhausted]
        <> [mkAbility a 2 $ forced $ ExhaustedWindow After (toTarget a)]
    _ -> []

instance RunMessage CaveTorch where
  runMessage msg o@(CaveTorch attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      dark <- select (LocationWithTrait Dark)
      chooseOrRunOneM pid do
        questionLabeled "caveTorch.location"
        causedBy attrs
        targets dark \l -> do
          -- no more than the location has room for
          left <- (-) <$> getLocationQuestPoints l <*> ((.progress) <$> getLocation l)
          chooseAmountM pid "caveTorch.progress" attrs 0 (max 0 $ min 3 left) \n -> do
            when (n > 0) $ placeProgress attrs l n
            exhaust attrs
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter ((== EnemyType) . cdCardType . toCardDef) cards
      pure o
    _ -> CaveTorch <$> runMessage msg attrs
