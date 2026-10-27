module LordOfTheRings.Location.Cards.TheDimrillStair (theDimrillStair) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheDimrillStair = TheDimrillStair LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDimrillStair :: LocationCard TheDimrillStair
theDimrillStair = location TheDimrillStair Cards.theDimrillStair

-- | Travel: Reshuffle all locations in the discard pile and victory display back into the
-- encounter deck. If you reshuffled at least two locations, reduce each player's threat
-- by 11 and discard all copies of Freezing Cold from play.
instance RunMessage TheDimrillStair where
  runMessage msg l@(TheDimrillStair a) = case msg of
    TravelTo lid | lid == a.id -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let isLocation = cardMatches (CardWithType LocationType)
          reshuffled =
            length (filter isLocation scenario.encounterDiscard)
              + length [c | EncounterCard c <- scenario.victoryDisplay, isLocation c]
      shuffleEncounterDiscardIntoDeck (CardWithType LocationType)
      shuffleVictoryDisplayIntoEncounterDeck (CardWithType LocationType)
      when (reshuffled >= 2) do
        eachPlayer (`lowerThreat` 11)
        colds <- getsGame (filter ((== toCardCode Treacheries.freezingCold) . toCardCode) . toList . (.entities.treacheries))
        for_ colds discard
      pure l
    _ -> TheDimrillStair <$> runMessage msg a
