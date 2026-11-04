module LordOfTheRings.Location.Cards.ShiftingQuagmire (shiftingQuagmire) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (locationsInVictoryDisplay)
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype ShiftingQuagmire = ShiftingQuagmire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shiftingQuagmire :: LocationCard ShiftingQuagmire
shiftingQuagmire = location ShiftingQuagmire Cards.shiftingQuagmire

-- | Travel: Spend 1 Fellowship resource to travel here: not without one to spend.
instance HasModifiersFor ShiftingQuagmire where
  getModifiersFor (ShiftingQuagmire a) = do
    payers <- fellowshipPayers
    when (null payers) $ modifySelf a [CannotBeTraveledTo]

fellowshipPayers :: HasGame m => m [PlayerId]
fellowshipPayers = filterM (\p -> (> 0) <$> getResourcesOfSphere p Fellowship) =<< getRemainingPlayers

-- | Surge. Mire 1.
-- Forced: When Shifting Quagmire is discarded by the Mire keyword, either return a random
-- location from the victory display to the staging area, or raise each player's threat by
-- 1 for each location in the victory display.
instance HasAbilities ShiftingQuagmire where
  getAbilities (ShiftingQuagmire a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

-- | The first player chooses; with no location in the victory display only the threat can
-- be raised, and that by nothing (FAQ 1.44: an option that can be done in full must be
-- chosen).
instance RunMessage ShiftingQuagmire where
  runMessage msg l@(ShiftingQuagmire a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      payers <- fellowshipPayers
      chooseOrRunOneM lead do
        questionLabeled "shiftingQuagmire.spend"
        causedBy a
        targets payers \p -> spendResources p Fellowship 1
      ShiftingQuagmire <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      victory <- locationsInVictoryDisplay
      chooseOrRunOneM lead do
        questionLabeled "shiftingQuagmire.choose"
        causedBy a
        for_ (nonEmpty victory) \cards -> labeled "shiftingQuagmire.return" do
          card <- uniform cards
          removeFromVictoryDisplay (EncounterCard card)
          addToStaging card
        labeled "shiftingQuagmire.raiseThreat"
          $ when (notNull victory)
          $ eachPlayer \pid -> encounterRaisesThreat pid (length victory)
      pure l
    _ -> ShiftingQuagmire <$> runMessage msg a
