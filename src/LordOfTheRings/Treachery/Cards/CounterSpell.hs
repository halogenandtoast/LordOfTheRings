module LordOfTheRings.Treachery.Cards.CounterSpell (counterSpell) where

import LordOfTheRings.Card (CardType (TreacheryType), cdCardType, toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CounterSpell = CounterSpell TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

counterSpell :: TreacheryCard CounterSpell
counterSpell = treachery CounterSpell Cards.counterSpell

-- | Counts as a Condition attachment with the text: "Forced: When a player plays an event,
-- discard the top card of the encounter deck. If that card is a treachery card, cancel the
-- effects of the event, discard the event player's hand, and discard Counter-spell from play."
instance HasAbilities CounterSpell where
  getAbilities (CounterSpell a) = [mkAbility a 1 $ forced $ EventPlayedWindow When | AttachedTo _ <- [a.placement]]

-- | When Revealed: Attach Counter-spell to Durin's Bane.
instance RunMessage CounterSpell where
  runMessage msg t@(CounterSpell attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attachToDurinsBane attrs
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      top <- getTopOfEncounterDeck
      discardTopOfEncounterDeck attrs 1
      when (maybe False ((== TreacheryType) . cdCardType . toCardDef) top) do
        for_ [(p, c) | Window _ (PlayingEvent p c) <- ws] \(p, c) -> do
          cancelEvent c
          discardHand p
        discard attrs
      pure t
    _ -> CounterSpell <$> runMessage msg attrs
