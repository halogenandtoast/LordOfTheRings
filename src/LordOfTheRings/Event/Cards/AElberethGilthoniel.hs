module LordOfTheRings.Event.Cards.AElberethGilthoniel (aElberethGilthoniel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEngagementCost)

newtype AElberethGilthoniel = AElberethGilthoniel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 4, on its card def.
-- Response: After a non-unique enemy attacks you, put that enemy on the bottom of the
-- encounter deck. If your threat is lower than that enemy's engagement cost, set your
-- threat equal to the engagement cost of that enemy.
aElberethGilthoniel :: EventCard AElberethGilthoniel
aElberethGilthoniel =
  eventWith AElberethGilthoniel Cards.aElberethGilthoniel \a ->
    a & windowL ?~ EnemyAttacksPlayerWindow After a.owner (not_ UniqueEnemy)

instance RunMessage AElberethGilthoniel where
  runMessage msg e@(AElberethGilthoniel attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [eid | Window _ (EnemyAttacking eid _) <- windows]) \eid -> do
        cost <- getEngagementCost eid
        threat <- (.threat) <$> getPlayer pid
        -- its shadow cards go to the discard pile, not with it
        push $ DiscardShadowCards eid
        returnToBottomOfEncounterDeck eid
        when (threat < cost) $ raiseThreat pid (cost - threat)
      pure e
    _ -> AElberethGilthoniel <$> runMessage msg attrs
