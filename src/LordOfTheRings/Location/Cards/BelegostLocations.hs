module LordOfTheRings.Location.Cards.BelegostLocations
  ( naurlhugsLair, abandonedMineBelegost, floodedHall, darkenedTunnel, sunkenTreasury, secretChamber ) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (AllyType), toCardCode)
import LordOfTheRings.Keyword (Keyword (Loot))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (isLoot, lootControlled, lootObjectives)
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (stalkingObjectives, placeStalkingResources)
import LordOfTheRings.Trait (Trait (Hazard))

newtype BelegostLocations = BelegostLocations LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

naurlhugsLair, abandonedMineBelegost, floodedHall, darkenedTunnel, sunkenTreasury, secretChamber :: LocationCard BelegostLocations
naurlhugsLair = location BelegostLocations Cards.naurlhugsLair
abandonedMineBelegost = location BelegostLocations Cards.abandonedMineBelegost
floodedHall = location BelegostLocations Cards.floodedHall
darkenedTunnel = location BelegostLocations Cards.darkenedTunnel
sunkenTreasury = location BelegostLocations Cards.sunkenTreasury
secretChamber = location BelegostLocations Cards.secretChamber

data BelegostLocationKind = Lair | Mine | Hall | Tunnel | Treasury | Chamber

locationKind :: LocationAttrs -> BelegostLocationKind
locationKind a
  | toCardCode a == toCardCode Cards.naurlhugsLair = Lair
  | toCardCode a == toCardCode Cards.abandonedMineBelegost = Mine
  | toCardCode a == toCardCode Cards.floodedHall = Hall
  | toCardCode a == toCardCode Cards.darkenedTunnel = Tunnel
  | toCardCode a == toCardCode Cards.sunkenTreasury = Treasury
  | otherwise = Chamber

instance HasModifiersFor BelegostLocations where
  getModifiersFor (BelegostLocations a) = case locationKind a of
    Lair -> do
      modifySelf a [ImmuneToPlayerCardEffects]
      whenM (null <$> stalkingObjectives) $ modifySelf a [CannotBeTraveledTo]
    Mine -> do
      players <- length <$> getPlayerOrder
      modifySelf a [AdditionalDiscoverCards players]
      when (a.placement == StagingArea) do
        locations <- select LocationInStagingArea
        for_ locations \lid -> do
          l <- getLocation lid
          unless (cardMatches (CardWithTitle "Abandoned Mine") l) $ modified a lid [ProgressLimit 0]
    Hall -> do
      players <- length <$> getPlayerOrder
      modifySelf a [AdditionalDiscoverHazards (max 0 $ players - 1)]
      when (a.placement == StagingArea) $ modified a GameTarget [IncreaseCost (CardWithType AllyType) 1]
    Tunnel -> whenM (null <$> stalkingObjectives) $ modifySelf a [CannotBeTraveledTo]
    Treasury -> do
      loot <- lootControlled
      modifySelf a [ThreatModifier loot]
      players <- traverse getPlayer =<< getRemainingPlayers
      when (any (null . (.hand)) players) $ modifySelf a [CannotBeTraveledTo]
    Chamber -> do
      loot <- lootObjectives
      when (any (\o -> o.placement == StagingArea && objectiveGuard o == Just a.cardId) loot) $ modifySelf a [CannotTriggerAbilities]

instance HasAbilities BelegostLocations where
  getAbilities (BelegostLocations a) = case locationKind a of
    Lair -> [mkAbility a 1 $ forced $ ExploredWindow When a.id]
    Tunnel -> [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]
    Chamber -> [restricted a 1 (CardInEncounterDiscard $ CardWithKeyword Loot)
      $ ResponseAbility (TraveledToWindow After $ LocationWithId a.id) Free]
    _ -> []

instance RunMessage BelegostLocations where
  runMessage msg l@(BelegostLocations a) = case msg of
    TravelTo lid | lid == a.id -> do
      case locationKind a of
        Lair -> placeStalkingResources a =<< length <$> getPlayerOrder
        Tunnel -> placeStalkingResources a 1
        Treasury -> do
          players <- getRemainingPlayers
          for_ players \pid -> discardRandomFromHand pid 1
        Chamber -> eachPlayer (`encounterRaisesThreat` 2)
        _ -> pure ()
      BelegostLocations <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      case locationKind a of
        Lair -> do
          clocks <- stalkingObjectives
          for_ clocks \o -> for_ [c | EncounterCard c <- o.objectiveUnderneath, isLoot c] \c -> do
            takeFromUnderneath o c.id
            addToStaging c
        Tunnel -> discardTopOfEncounterDeck a 1
        Chamber -> do
          lead <- getLead
          loot <- filter isLoot <$> getEncounterDiscard
          chooseOrRunOneM lead do
            questionLabeled "belegost.secretChamberLoot"
            causedBy a
            targets loot \card -> do
              removeFromEncounterDiscard card
              putObjectiveIntoPlayGuardedBy card a.cardId
        _ -> pure ()
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ cards \card -> when (cardMatches (CardWithTrait Hazard) card) $ revealFromEncounterDiscard card
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseAndExhaust a pid 1
      pure l
    _ -> BelegostLocations <$> runMessage msg a
