module LordOfTheRings.Event.Cards.TomBombadillo (tomBombadillo) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype TomBombadillo = TomBombadillo EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a Hobbit hero. You cannot play this card if there is a copy
-- of Tom Bombadillo! in the victory display.
-- Planning Action: Add Tom Bombadillo! to the victory display to shuffle a set-aside copy
-- of Tom Bombadil into the encounter deck.
--
-- The copy of Tom Bombadil may be any player's: each sets his own aside at setup (MEC78
-- rules, "Encounter Keyword"), and the card does not say whose.
tomBombadillo :: EventCard TomBombadillo
tomBombadillo =
  eventWith TomBombadillo Cards.tomBombadillo \a ->
    a
      & criteriaL
      .~ DuringPhase PlanningPhase
      <> exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Hobbit)
      <> NotCriterion (CardInVictoryDisplay $ cardIs Cards.tomBombadillo)

instance RunMessage TomBombadillo where
  runMessage msg e@(TomBombadillo attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      addToVictoryDisplay card
      scenario <- toAttrs <$> getsGame (.scenario)
      -- one already shuffled in is still its owner's, but no longer set aside
      let shuffled = map (.id) $ scenario.encounterDeck <> scenario.encounterDiscard
      toms <-
        filter (\c -> cardMatches (cardIs Allies.tomBombadil) c && c.id `notElem` shuffled)
          . concatMap (.setAsideCards)
          <$> (traverse getPlayer =<< getRemainingPlayers)
      -- the copies are alike: any one will do
      traverse_ shuffleIntoEncounterDeck (headMay toms)
      pure e
    _ -> TomBombadillo <$> runMessage msg attrs
