module LordOfTheRings.Event.Cards.RevealedInWrath (revealedInWrath) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor))

newtype RevealedInWrath = RevealedInWrath EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

wrathful :: EnemyMatcher
wrathful = not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards

-- | Play only if you control a Noldor hero.
-- Action: Choose a non-unique enemy. Until the end of the phase, treat the chosen enemy's
-- printed text box as if it were blank (except for Traits).
revealedInWrath :: EventCard RevealedInWrath
revealedInWrath =
  eventWith RevealedInWrath Cards.revealedInWrath \a ->
    a & criteriaL .~ exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Noldor) <> exists wrathful

instance RunMessage RevealedInWrath where
  runMessage msg e@(RevealedInWrath attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select wrathful
      chooseOrRunOneM pid do
        questionLabeled "revealedInWrath.enemy"
        causedBy attrs
        targets enemies \eid -> phaseModifier attrs eid Blank
      pure e
    _ -> RevealedInWrath <$> runMessage msg attrs
