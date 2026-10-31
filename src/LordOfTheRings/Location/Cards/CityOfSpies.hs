module LordOfTheRings.Location.Cards.CityOfSpies (cityOfSpies) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CityOfSpies = CityOfSpies LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cityOfSpies :: LocationCard CityOfSpies
cityOfSpies = location CityOfSpies Cards.cityOfSpies

-- | While City of Spies is in the staging area, it gains: "Forced: When you commit a
-- character to the quest, raise your threat by 1. This effect does not stack with other
-- copies of City of Spies."
--
-- Only the copy in the staging area with the least id has it, standing for them all.
instance HasAbilities CityOfSpies where
  getAbilities (CityOfSpies a) =
    [ restricted a 1 (NotCriterion $ LocationExists $ LocationInStagingArea <> LocationWithCardCode (toCardCode a) <> LocationWithIdBelow a.id)
      $ forced
      $ AnyCommittedToQuestWindow When
    | a.placement == StagingArea
    ]

instance RunMessage CityOfSpies where
  runMessage msg l@(CityOfSpies a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [c | Window _ (CommittedToQuest c) <- ws] \c ->
        characterController c >>= traverse_ \pid -> encounterRaisesThreat pid 1
      pure l
    _ -> CityOfSpies <$> runMessage msg a
