module LordOfTheRings.Event.Cards.CommonCause (commonCause) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype CommonCause = CommonCause EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust 1 hero you control to choose and ready a different hero.
commonCause :: EventCard CommonCause
commonCause =
  eventWith CommonCause Cards.commonCause \a ->
    a & criteriaL .~ (exists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero) <> exists ExhaustedHero)

instance RunMessage CommonCause where
  runMessage msg e@(CommonCause attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      readyHeroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      -- the hero exhausted is ready now, so it is never one of these
      exhausted <- select ExhaustedHero
      chooseOneM pid do
        questionLabeled "commonCause.exhaust"
        causedBy attrs
        targets readyHeroes \h -> do
          exhaust h
          chooseOneM pid do
            questionLabeled "commonCause.ready"
            causedBy attrs
            targets exhausted ready
      pure e
    _ -> CommonCause <$> runMessage msg attrs
