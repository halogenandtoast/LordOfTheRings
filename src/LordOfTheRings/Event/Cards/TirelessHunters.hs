module LordOfTheRings.Event.Cards.TirelessHunters (tirelessHunters) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype TirelessHunters = TirelessHunters EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

notEngagedWith :: PlayerId -> EnemyMatcher
notEngagedWith = not_ . EnemyEngagedWith . PlayerWithId

-- | Play only before the resolving enemy attacks step.
-- Combat Action: Choose an enemy not engaged with you. Engage that enemy. Then, discard a
-- shadow card from that enemy, if able.
tirelessHunters :: EventCard TirelessHunters
tirelessHunters =
  eventWith TirelessHunters Cards.tirelessHunters \a ->
    a
      & criteriaL
      .~ DuringPhase CombatPhase
      <> TargetLacksModifier GameTarget ResolvingEnemyAttacks
      <> exists (notEngagedWith a.owner)

instance RunMessage TirelessHunters where
  runMessage msg e@(TirelessHunters attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (notEngagedWith pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemyToEngage"
        causedBy attrs
        targets enemies \eid -> do
          engageEnemy pid eid
          shadows <- (.shadowCards) <$> getEnemy eid
          for_ (take 1 shadows) (discardShadowCard eid)
      pure e
    _ -> TirelessHunters <$> runMessage msg attrs
