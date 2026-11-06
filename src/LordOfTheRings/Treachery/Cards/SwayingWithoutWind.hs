module LordOfTheRings.Treachery.Cards.SwayingWithoutWind (swayingWithoutWind) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (victoryLocations)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwayingWithoutWind = SwayingWithoutWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swayingWithoutWind :: TreacheryCard SwayingWithoutWind
swayingWithoutWind = treachery SwayingWithoutWind Cards.swayingWithoutWind

instance RunMessage SwayingWithoutWind where
  runMessage msg t@(SwayingWithoutWind a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pid <- getRevealer
      locations <- victoryLocations
      if null locations
        then phaseModifier a a (GainKeyword Surge)
        else chooseOrRunOneM pid do
          questionLabeled "oldForest.returnLocation"
          causedBy a
          targets locations \card -> do
            removeFromVictoryDisplay (EncounterCard card)
            addToStaging card
      pure t
    _ -> SwayingWithoutWind <$> runMessage msg a
