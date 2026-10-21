module LordOfTheRings.Event.Cards.TheMusterOfRohan (theMusterOfRohan) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Sphere (Sphere (Spirit))
import LordOfTheRings.Trait (Trait (Rohan))

newtype TheMusterOfRohan = TheMusterOfRohan EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Planning Action: Search the top 10 cards of your deck for up to 4 Rohan allies and put
-- them into play. Shuffle your deck. If any of those allies are still in play at the end
-- of the round, discard them.
theMusterOfRohan :: EventCard TheMusterOfRohan
theMusterOfRohan = eventWith TheMusterOfRohan Cards.theMusterOfRohan (criteriaL .~ DuringPhase PlanningPhase)

-- | While paying for The Muster of Rohan, each Rohan hero you control is considered to
-- have a spirit icon.
instance HasModifiersFor TheMusterOfRohan where
  getModifiersFor (TheMusterOfRohan a) = do
    heroes <- select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Rohan)
    for_ heroes \h -> modified a h [GainResourceIconFor (cardIs Cards.theMusterOfRohan) Spirit]

instance RunMessage TheMusterOfRohan where
  runMessage msg e@(TheMusterOfRohan attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeckIntoPlay pid attrs 10 (CardWithTrait Rohan <> CardWithType AllyType) 4 [DiscardAtEndOfRound]
      pure e
    _ -> TheMusterOfRohan <$> runMessage msg attrs
