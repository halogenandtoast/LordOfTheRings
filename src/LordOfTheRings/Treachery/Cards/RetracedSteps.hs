module LordOfTheRings.Treachery.Cards.RetracedSteps (retracedSteps) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (..), CardType (LocationType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Trait (Trait (EmynMuil))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RetracedSteps = RetracedSteps TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

retracedSteps :: TreacheryCard RetracedSteps
retracedSteps = treachery RetracedSteps Cards.retracedSteps

-- | Surge.
-- When Revealed: Discard a random Emyn Muil location from the victory display.
instance RunMessage RetracedSteps where
  runMessage msg t@(RetracedSteps attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      victory <- getsGame (scenarioVictoryDisplay . toAttrs . gameScenario)
      let emynMuil def = cdCardType def == LocationType && EmynMuil `member` cdCardTraits def
      case [c | EncounterCard c <- victory, emynMuil (toCardDef c)] of
        [] -> pure ()
        found -> do
          c <- uniform found
          removeFromVictoryDisplay (EncounterCard c)
          push $ AddToEncounterDiscard [c]
      pure t
    _ -> RetracedSteps <$> runMessage msg attrs
