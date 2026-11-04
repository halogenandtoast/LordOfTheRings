module LordOfTheRings.Event.Cards.TheGatheringStorm (theGatheringStorm) where

import LordOfTheRings.Card (Card (EncounterCard), cdThreat, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype TheGatheringStorm = TheGatheringStorm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Planning Action: Search the top 5 cards of the encounter deck for an enemy. Add that
-- enemy to the staging area to reduce the cost of the first ally you play this phase by X,
-- where X is that enemy's threat. Shuffle the encounter deck.
--
-- X is the enemy's printed threat, read from the card as it is added.
theGatheringStorm :: EventCard TheGatheringStorm
theGatheringStorm = eventWith TheGatheringStorm Cards.theGatheringStorm (criteriaL .~ DuringPhase PlanningPhase)

instance RunMessage TheGatheringStorm where
  runMessage msg e@(TheGatheringStorm attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getTopOfEncounterDeckN 5
      chooseOneM pid do
        questionLabeled "theGatheringStorm.enemy"
        causedBy attrs
        showing (map EncounterCard top)
        targets (filter (cardMatches $ CardWithType EnemyType) top) \c -> do
          removeFromEncounterDeck c
          addToStaging c
          phaseModifier attrs (PlayerTarget pid) (LowerNextCostOf (CardWithType AllyType) (fromMaybe 0 $ cdThreat $ toCardDef c))
        labeled "theGatheringStorm.none" (pure ())
      shuffleEncounterDeck
      pure e
    _ -> TheGatheringStorm <$> runMessage msg attrs
