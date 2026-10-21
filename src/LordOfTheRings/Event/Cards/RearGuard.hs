module LordOfTheRings.Event.Cards.RearGuard (rearGuard) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype RearGuard = RearGuard EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action: Discard a Leadership ally to give each hero committed to this quest +1
-- willpower until the end of the phase.
rearGuard :: EventCard RearGuard
rearGuard =
  eventWith RearGuard Cards.rearGuard \a ->
    a & criteriaL .~ (DuringPhase QuestPhase <> exists (AllyControlledBy (PlayerWithId a.owner) <> AllyWithSphere Leadership))

instance RunMessage RearGuard where
  runMessage msg e@(RearGuard attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> AllyWithSphere Leadership)
      chooseOneM pid do
        questionLabeled "rearGuard.discard"
        causedBy attrs
        targets allies \x -> do
          discard x
          heroes <- select CommittedHero
          for_ heroes \h -> phaseModifier attrs h (WillpowerModifier 1)
      pure e
    _ -> RearGuard <$> runMessage msg attrs
