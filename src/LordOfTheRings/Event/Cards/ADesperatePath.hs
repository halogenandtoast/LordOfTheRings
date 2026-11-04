module LordOfTheRings.Event.Cards.ADesperatePath (aDesperatePath) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ADesperatePath = ADesperatePath EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

aDesperatePath :: EventCard ADesperatePath
aDesperatePath = eventWith ADesperatePath Cards.aDesperatePath \a -> a & windowL ?~ StagingStepEndsWindow

-- | Response: At the end of the staging step, discard cards from the top of the encounter
-- deck until you discard a treachery. Resolve its 'when revealed' effects to ready each of
-- your questing characters. Each of your questing characters gets +1 willpower until the
-- end of the phase.
--
-- Resolving the treachery is the cost of the rest: with none discarded, nothing more
-- happens. Your questing characters are read once its effects have resolved.
instance RunMessage ADesperatePath where
  runMessage msg e@(ADesperatePath attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      discardEncounterCardsUntilThen attrs (CardWithType TreacheryType) \treachery -> do
        resolveWhenRevealedFromDiscard treachery
        doStep 1 msg
      pure e
    DoStep 1 (PlayEvent pid card _) | card.id == toId attrs -> do
      let you = PlayerWithId pid
      questing <- selectCharacters (HeroControlledBy you <> CommittedHero) (AllyControlledBy you <> CommittedAlly)
      for_ questing ready
      for_ questing \c -> phaseModifier attrs c (WillpowerModifier 1)
      pure e
    _ -> ADesperatePath <$> runMessage msg attrs
