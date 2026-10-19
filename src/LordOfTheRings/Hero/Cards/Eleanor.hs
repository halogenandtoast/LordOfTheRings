module LordOfTheRings.Hero.Cards.Eleanor (eleanor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Eleanor = Eleanor HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eleanor :: HeroCard Eleanor
eleanor = hero Eleanor Cards.eleanor

-- | Response: Exhaust Eleanor to cancel the "when revealed" effects of a treachery card
-- just revealed by the encounter deck. Then, discard that card, and replace it with the
-- next card from the encounter deck.
instance HasAbilities Eleanor where
  getAbilities (Eleanor a) = [mkAbility a 1 $ ResponseAbility (TreacheryRevealedWindow When) (ExhaustCost $ toTarget a)]

instance RunMessage Eleanor where
  runMessage msg h@(Eleanor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [(t, tid) | Window _ (Revealed t@(TreacheryTarget tid)) <- windows] \(t, tid) -> do
        cancelWhenRevealed t
        cancelMatching (== FinishRevelation tid)
        discard t
        revealEncounterCard
      pure h
    _ -> Eleanor <$> runMessage msg attrs
