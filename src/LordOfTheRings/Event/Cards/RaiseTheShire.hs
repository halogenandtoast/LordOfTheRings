module LordOfTheRings.Event.Cards.RaiseTheShire (raiseTheShire) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype RaiseTheShire = RaiseTheShire EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After you engage an enemy, search the top 5 cards of your deck for a Hobbit
-- ally and put it into play (search your entire deck instead if that enemy's engagement
-- cost is higher than your threat). Shuffle your deck. At the end of the round, if that
-- ally is still in play, return it to your hand.
raiseTheShire :: EventCard RaiseTheShire
raiseTheShire = eventWith RaiseTheShire Cards.raiseTheShire \a -> a & windowL ?~ PlayerEngagedEnemyWindow After a.owner

instance RunMessage RaiseTheShire where
  runMessage msg e@(RaiseTheShire attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      engaged <- filterM (\eid -> selectAny (EnemyWithId eid <> EnemyWithEngagementCostAboveThreatOf pid))
        [eid | Window _ (EnemyEngaged eid p) <- windows, p == pid]
      deck <- (.deck) <$> getPlayer pid
      let n = if null engaged then 5 else length deck
      searchDeckIntoPlay pid attrs n (CardWithType AllyType <> CardWithTrait Hobbit) 1 [ReturnToHandAtEndOfRound]
      pure e
    _ -> RaiseTheShire <$> runMessage msg attrs
