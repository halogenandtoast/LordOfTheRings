module LordOfTheRings.Quest.Cards.RetakeTheCity (retakeTheCity) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers
import LordOfTheRings.Trait (Trait (Osgiliath))

newtype RetakeTheCity = RetakeTheCity QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

retakeTheCity :: QuestCard RetakeTheCity
retakeTheCity = quest RetakeTheCity Cards.retakeTheCity

-- | Player card effects cannot place progress tokens on locations in the staging area.
instance HasModifiersFor RetakeTheCity where
  getModifiersFor (RetakeTheCity a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [NoProgressFromPlayerCards]

-- | If the players control all Osgiliath locations in play at the end of the round, they
-- have won the game.
instance HasAbilities RetakeTheCity where
  getAbilities (RetakeTheCity a) =
    [ restricted a 1 (NotCriterion $ exists $ LocationWithTrait Osgiliath <> not_ (LocationControlledBy AnyPlayer))
        $ forced RoundEndsWindow
    | a.flipped
    ]

-- | Setup: Each player chooses 1 enemy and 1 unique location and add them to the staging
-- area. Shuffle the encounter deck.
-- Forced: When an Osgiliath location leaves play as an explored location, the first
-- player takes control of that location.
instance RunMessage RetakeTheCity where
  runMessage msg q@(RetakeTheCity attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      -- each search shuffles the encounter deck after it
      eachPlayer \p -> do
        searchEncounterDeckToStaging p (CardWithType EnemyType)
        searchEncounterDeckToStaging p uniqueLocations
      turnOver attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      winGame
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [lid | Window When (Explored lid) <- ws] \lid ->
        whenM (selectAny $ LocationWithId lid <> LocationWithTrait Osgiliath) do
          cancelMatching \case
            Discard (LocationTarget l) -> l == lid
            AddToVictoryDisplay (LocationTarget l) -> l == lid
            _ -> False
          lead <- getLead
          takeControlOfLocation lead lid
      RetakeTheCity <$> runMessage msg attrs
    -- an eliminated player's Osgiliath locations return to the staging area (rules sheet)
    EliminatePlayer pid -> do
      traverse_ returnToStaging =<< controlledLocations pid
      RetakeTheCity <$> runMessage msg attrs
    _ -> RetakeTheCity <$> runMessage msg attrs
   where
    uniqueLocations =
      CardMatchAny $ map cardIs [Locations.westGate, Locations.theKingsLibrary, Locations.ancientHarbor, Locations.theOldBridge]
