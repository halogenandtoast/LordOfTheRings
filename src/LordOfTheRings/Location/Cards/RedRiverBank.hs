module LordOfTheRings.Location.Cards.RedRiverBank (redRiverBank) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype RedRiverBank = RedRiverBank LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

redRiverBank :: LocationCard RedRiverBank
redRiverBank = location RedRiverBank Cards.redRiverBank

-- | The discarded card's printed threat is counted while it is out of play (X is 0).
instance HasAbilities RedRiverBank where
  getAbilities (RedRiverBank a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After RefreshPhase]

instance RunMessage RedRiverBank where
  runMessage msg l@(RedRiverBank attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      top <- headMay <$> getEncounterDiscard
      for_ top \c -> do
        moveDiscardToBottomOfEncounterDeck 1
        eachPlayer \p -> encounterRaisesThreat p (fromMaybe 0 $ cdThreat $ toCardDef c)
      pure l
    _ -> RedRiverBank <$> runMessage msg attrs
