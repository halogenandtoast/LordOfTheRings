module LordOfTheRings.Enemy.Cards.HeavyCruiser (heavyCruiser) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (dolAmrothObjectivesToSearch, objectiveWithCardId)

newtype HeavyCruiser = HeavyCruiser EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

heavyCruiser :: EnemyCard HeavyCruiser
heavyCruiser = enemy HeavyCruiser Cards.heavyCruiser

-- | Boarding 2. Cannot have attachments.
instance HasModifiersFor HeavyCruiser where
  getModifiersFor (HeavyCruiser a) = modified a a [CannotHaveAttachments]

-- | When Revealed: Search the encounter deck and discard pile for a Dol Amroth objective
-- and add it to the staging area. Then, attach Heavy Cruiser to that objective, guarding
-- it. Shuffle the encounter deck.
instance RunMessage HeavyCruiser where
  runMessage msg e@(HeavyCruiser attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      found <- dolAmrothObjectivesToSearch
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "heavyCruiser.objective"
        causedBy attrs
        targets found \card -> do
          removeFromEncounterDeck card
          removeFromEncounterDiscard card
          addToStaging card
          doStep 1 (Run [msg, AddToStaging card])
      shuffleEncounterDeck
      pure e
    -- the objective it found is now in the staging area
    DoStep 1 (Run [WhenRevealed (isTarget attrs -> True), AddToStaging card]) -> do
      objectiveWithCardId card.id >>= traverse_ (`guardObjectiveWith` attrs.cardId)
      pure e
    _ -> HeavyCruiser <$> runMessage msg attrs
