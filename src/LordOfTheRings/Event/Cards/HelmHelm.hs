module LordOfTheRings.Event.Cards.HelmHelm (helmHelm) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Rohan))

newtype HelmHelm = HelmHelm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only after the resolving enemy attacks step is complete. Playable with a ready
-- Rohan ally to exhaust and a non-unique enemy engaged with you.
helmHelm :: EventCard HelmHelm
helmHelm =
  eventWith HelmHelm Cards.helmHelm \a ->
    a
      & criteriaL
      .~ DuringPhase CombatPhase
      <> TargetHasModifier GameTarget EnemyAttacksStepComplete
      <> exists (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Rohan <> ReadyAlly)
      <> exists (EnemyEngagedWith (PlayerWithId a.owner) <> NotEnemy UniqueEnemy)

-- | Combat Action: Exhaust and discard a Rohan ally you control to choose and discard a
-- non-unique enemy engaged with you.
instance RunMessage HelmHelm where
  runMessage msg e@(HelmHelm attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Rohan <> ReadyAlly)
      enemies <- select (EnemyEngagedWith (PlayerWithId pid) <> NotEnemy UniqueEnemy)
      chooseOrRunOneM pid do
        questionLabeled "helmHelm.ally"
        causedBy attrs
        targets allies \al -> do
          exhaust al
          discard al
          chooseOrRunOneM pid do
            questionLabeled "helmHelm.enemy"
            causedBy attrs
            targets enemies discard
      pure e
    _ -> HelmHelm <$> runMessage msg attrs
