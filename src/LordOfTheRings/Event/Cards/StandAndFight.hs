module LordOfTheRings.Event.Cards.StandAndFight (standAndFight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype StandAndFight = StandAndFight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an ally with a printed cost of X in any player's discard pile. Put that
-- ally into play under your control. (The chosen ally can belong to any sphere.)
--
-- X is paid in Spirit resources once the ally is chosen, so only allies the player can
-- pay for are offered, and with none it can't be played.
standAndFight :: EventCard StandAndFight
standAndFight = eventWith StandAndFight Cards.standAndFight \a -> a & criteriaL .~ AffordableAllyInDiscard a.eventOwner Spirit

instance RunMessage StandAndFight where
  runMessage msg e@(StandAndFight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- affordableDiscardedAllies pid Spirit
      chooseOneM pid do
        questionLabeled "chooseAllyToPutIntoPlay"
        causedBy attrs
        for_ allies \(owner, pc, x) -> targeting pc do
          spendResources pid Spirit x
          removeFromDiscard owner pc.id
          push $ PutAllyIntoPlayUnder pid pc
      pure e
    _ -> StandAndFight <$> runMessage msg attrs
