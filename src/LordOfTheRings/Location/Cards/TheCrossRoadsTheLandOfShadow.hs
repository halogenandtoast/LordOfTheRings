module LordOfTheRings.Location.Cards.TheCrossRoadsTheLandOfShadow (theCrossRoadsTheLandOfShadow) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheCrossRoadsTheLandOfShadow = TheCrossRoadsTheLandOfShadow LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossRoadsTheLandOfShadow :: LocationCard TheCrossRoadsTheLandOfShadow
theCrossRoadsTheLandOfShadow = location TheCrossRoadsTheLandOfShadow Cards.theCrossRoadsTheLandOfShadow

-- | Immune to player card effects.
-- The Cross-roads gets -1 threat for each facedown card under it. The players cannot
-- travel here unless there are no facedown cards under The Cross-roads and at least 20
-- progress tokens on stage 1B.
instance HasModifiersFor TheCrossRoadsTheLandOfShadow where
  getModifiersFor (TheCrossRoadsTheLandOfShadow a) = do
    progress <- getMainQuestProgress
    let n = length a.underneath
    modifySelf a $ ImmuneToPlayerCardEffects : [ThreatModifier (negate n) | n > 0] <> [CannotBeTraveledTo | n > 0 || progress < 20]

-- | The top facedown card underneath it is revealed, added to the staging area or put into
-- play engaged with the player given (see
-- 'LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers.revealFromUnderTheCrossRoads').
instance RunMessage TheCrossRoadsTheLandOfShadow where
  runMessage msg l@(TheCrossRoadsTheLandOfShadow a) = case msg of
    ScenarioSpecific "theCrossRoads.put" (parseMaybe parseJSON -> Just pid) -> case a.underneath of
      EncounterCard card : rest -> do
        push $ PutEnemyIntoPlayEngaged pid card
        pure $ TheCrossRoadsTheLandOfShadow a {locationUnderneath = rest}
      _ -> pure l
    ScenarioSpecific "theCrossRoads.reveal" (parseMaybe parseJSON -> Just engaged) -> case a.underneath of
      EncounterCard card : rest -> do
        push $ maybe (RevealCard card) (`RevealCardEngaged` card) engaged
        pure $ TheCrossRoadsTheLandOfShadow a {locationUnderneath = rest}
      _ -> pure l
    _ -> TheCrossRoadsTheLandOfShadow <$> runMessage msg a
