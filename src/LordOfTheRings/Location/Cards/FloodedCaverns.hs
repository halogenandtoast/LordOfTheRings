module LordOfTheRings.Location.Cards.FloodedCaverns (floodedCaverns) where

import LordOfTheRings.Card (Card (EncounterCard), cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Trait (Trait (Underwater))

newtype FloodedCaverns = FloodedCaverns LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

floodedCaverns :: LocationCard FloodedCaverns
floodedCaverns = location FloodedCaverns Cards.floodedCaverns

-- | If Flooded Caverns is in the staging area and the active location is Underwater, add
-- that location's threat to the threat of Flooded Caverns.
instance HasModifiersFor FloodedCaverns where
  getModifiersFor (FloodedCaverns a) = when (a.placement == StagingArea) do
    underwater <- select (IsActiveLocation <> LocationWithTrait Underwater)
    x <- sum <$> traverse getLocationThreat underwater
    when (x > 0) $ modifySelf a [ThreatModifier x]

-- | Forced: If Flooded Caverns is the active location at the end of the quest phase, return
-- an Underwater location from the victory display to the bottom of the Grotto deck, Grotto
-- side faceup. Then, return Flooded Caverns to the staging area.
instance HasAbilities FloodedCaverns where
  getAbilities (FloodedCaverns a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

instance RunMessage FloodedCaverns where
  runMessage msg l@(FloodedCaverns a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      victory <- getsGame (scenarioVictoryDisplay . toAttrs . gameScenario)
      let drowned = [c | EncounterCard c <- victory, Underwater `member` cdCardTraits (toCardDef c)]
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "floodedCaverns.return"
        causedBy a
        targets drowned \c -> do
          removeFromVictoryDisplay (EncounterCard c)
          push $ PutOnBottomOfGrottoDeck c
      placeLocation a.id StagingArea
      pure l
    _ -> FloodedCaverns <$> runMessage msg a
