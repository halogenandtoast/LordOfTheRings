module LordOfTheRings.Location.Cards.TrollFells (trollFells) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, AttachedTo))
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (guardedBy)
import LordOfTheRings.Trait (Trait (Weather))

newtype TrollFells = TrollFells LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollFells :: LocationCard TrollFells
trollFells = location TrollFells Cards.trollFells

-- | While Troll-fells is the active location, Weather cards gain doomed 1: the doomed of
-- a Weather card revealed, after its effects.
instance HasAbilities TrollFells where
  getAbilities (TrollFells a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Weather | a.placement == ActiveLocation]

-- | When Revealed: If Troll-fells is not revealed guarding a safe location, search the
-- encounter deck and discard pile for a location with the safe keyword and attach it to
-- Troll-fells, guarded. Shuffle the encounter deck.
instance RunMessage TrollFells where
  runMessage msg l@(TrollFells a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      raiseThreatFromDoomed 1
      pure l
    WhenRevealed (isTarget a -> True) -> do
      guarding <- notNull <$> guardedBy a.cardId
      unless guarding do
        lead <- getLead
        scenario <- getsGame (toAttrs . (.scenario))
        let safe = filter (member Safe . cdKeywords . toCardDef)
            fromDeck = safe scenario.encounterDeck
            fromDiscard = safe scenario.encounterDiscard
            guard' remove c = targeting c do
              remove c
              putLocationIntoPlay c (AttachedTo $ CardIdTarget a.cardId)
        chooseOneM lead do
          questionLabeled "trollFells.search"
          causedBy a
          showing (map EncounterCard $ scenario.encounterDeck <> scenario.encounterDiscard)
          for_ fromDeck $ guard' removeFromEncounterDeck
          for_ fromDiscard $ guard' removeFromEncounterDiscard
        shuffleEncounterDeck
      pure l
    _ -> TrollFells <$> runMessage msg a
