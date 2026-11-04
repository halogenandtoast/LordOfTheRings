module LordOfTheRings.Quest.Cards.SlavesOfTheDarkLord (slavesOfTheDarkLord) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (campsInStaging, captivesControlled)
import LordOfTheRings.Trait (Trait (Camp, Captive, Warden))

newtype SlavesOfTheDarkLord = SlavesOfTheDarkLord QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

slavesOfTheDarkLord :: QuestCard SlavesOfTheDarkLord
slavesOfTheDarkLord = quest SlavesOfTheDarkLord Cards.slavesOfTheDarkLord

-- | Each Camp location is immune to player card effects, and players cannot travel there
-- while a Warden enemy is in play.
instance HasModifiersFor SlavesOfTheDarkLord where
  getModifiersFor (SlavesOfTheDarkLord a) = when a.flipped do
    camps <- select (LocationWithTrait Camp)
    warden <- selectAny (EnemyWithTrait Warden)
    for_ camps \l -> modified a l $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | warden]

-- | Forced: At the end of the round, raise each player's threat by 1 for each Camp location
-- in the staging area.
instance HasAbilities SlavesOfTheDarkLord where
  getAbilities (SlavesOfTheDarkLord a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Add each Camp location to the staging area. Attach a random Captive objective-ally
to each Camp location as a guarded objective. Place 1 random Warden enemy faceup (but not
in play) under each Camp location. Shuffle the encounter deck and discard cards from the
top until X locations are discarded, where X is 1 less than the number of players. Add
each location discarded this way to the staging area. (The scenario sets the Camps,
Captives and Wardens aside as it gathers.)

This stage cannot be defeated unless players control all 3 Captive objective-allies.
-}
instance RunMessage SlavesOfTheDarkLord where
  runMessage msg q@(SlavesOfTheDarkLord attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (CardWithTrait Camp)
      doStep 1 msg
      SlavesOfTheDarkLord <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      camps <- select (LocationWithTrait Camp)
      captives <- shuffleM =<< getSetAside (CardWithTrait Captive)
      wardens <- shuffleM =<< getSetAside (CardWithTrait Warden)
      for_ (zip3 camps captives wardens) \(camp, captive, warden) -> do
        guardCardWith (EncounterCard captive) =<< getLocation camp
        placeUnderneath camp [EncounterCard warden]
      shuffleEncounterDeck
      doStep 2 (PerformSetup $ toTarget attrs)
      pure q
    DoStep 2 (PerformSetup (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs (players - 1) (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      camps <- campsInStaging
      eachPlayer \pid -> encounterRaisesThreat pid camps
      pure q
    _ -> SlavesOfTheDarkLord <$> runConditionalStage ((== 3) <$> captivesControlled) msg attrs
