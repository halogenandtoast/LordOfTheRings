module LordOfTheRings.Event.Cards.HostOfGaladhrim (hostOfGaladhrim) where

import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Silvan))

newtype HostOfGaladhrim = HostOfGaladhrim EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Planning Action: Return each Silvan ally you control to your hand. Then, play each of
-- those allies from your hand one at a time at no cost.
--
-- ponytail: they are played in the order they were in play; choosing the order can come
-- with a card that cares about it.
hostOfGaladhrim :: EventCard HostOfGaladhrim
hostOfGaladhrim = eventWith HostOfGaladhrim Cards.hostOfGaladhrim (criteriaL .~ DuringPhase PlanningPhase)

instance RunMessage HostOfGaladhrim where
  runMessage msg e@(HostOfGaladhrim attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      silvan <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Silvan)
      cards <- traverse (fmap (allyCardId . toAttrs) . getAlly) silvan
      for_ silvan returnToHand
      for_ cards (playFromHandAtNoCost pid)
      pure e
    _ -> HostOfGaladhrim <$> runMessage msg attrs
