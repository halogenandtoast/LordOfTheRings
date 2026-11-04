module LordOfTheRings.Event.Cards.QuickEars (quickEars) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Ranger))

newtype QuickEars = QuickEars EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

listeners :: PlayerId -> HeroMatcher
listeners pid = HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroMatchAny [HeroWithTrait Dunedain, HeroWithTrait Ranger]

-- | Response: Exhaust a Dúnedain or Ranger hero to cancel an enemy card just revealed
-- from the encounter deck. Then, shuffle it back into the encounter deck and reveal an
-- additional encounter card.
quickEars :: EventCard QuickEars
quickEars =
  eventWith QuickEars Cards.quickEars \a ->
    a
      & windowL ?~ EnemyRevealedWindow When
      & criteriaL .~ (EncounterEffectsCancelable <> exists (listeners a.owner))

instance RunMessage QuickEars where
  runMessage msg e@(QuickEars attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      heroes <- select (listeners pid)
      chooseOrRunOneM pid do
        questionLabeled "quickEars.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          -- what is left of its revelation waits on it being in play
          for_ (take 1 [t | Window _ (Revealed t@(EnemyTarget _)) <- windows]) \t -> do
            cancelWhenRevealed pid t
            shuffleIntoEncounterDeck t
            revealEncounterCard
      pure e
    _ -> QuickEars <$> runMessage msg attrs
