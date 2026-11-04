module LordOfTheRings.Quest.Cards.TakeTheShip (takeTheShip) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype TakeTheShip = TakeTheShip QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

takeTheShip :: QuestCard TakeTheShip
takeTheShip = quest TakeTheShip Cards.takeTheShip

elite :: [EnemyMatcher]
elite = map (EnemyWithCardCode . toCardCode) [Enemies.captainSahir, Enemies.naasiyah]

-- | Captain Sahír and Na'asiyah gain indestructible.
instance HasModifiersFor TakeTheShip where
  getModifiersFor (TakeTheShip a) = when a.flipped do
    enemies <- concatMapM select elite
    for_ enemies \e -> modified a e [GainKeyword Indestructible]

-- | Forced: At the end of the round, if Captain Sahír and Na'asiyah are not in play, add 1
-- resource to each of them.
-- The resources go on their set-aside cards, coming with them into play.
instance HasAbilities TakeTheShip where
  getAbilities (TakeTheShip a) =
    [restricted a 1 (Criteria $ map (NotCriterion . EnemyExists) elite) $ forced RoundEndsWindow | a.flipped]

-- | Setup: Build the encounter deck using only The Stormcaller and Corsair Raiders
-- encounter sets, setting The Thing in the Depths and The Stormcaller Elite encounter sets
-- aside, out of play (enemy side faceup). Search the encounter deck for Helm of the
-- Stormcaller and 1 copy of Umbar Raider, and add them to the staging area. The scenario
-- built the deck and set the rest aside.
-- Progress cannot be placed on this stage unless Helm of the Stormcaller is in the victory
-- display.
instance RunMessage TakeTheShip where
  runMessage msg q@(TakeTheShip attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      deck <- getsGame ((.scenarioEncounterDeck) . toAttrs . gameScenario)
      let firstOf def = take 1 [c | c <- deck, toCardCode c == toCardCode def]
      for_ (firstOf Locations.helmOfTheStormcaller <> firstOf Enemies.umbarRaider) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      TakeTheShip <$> runMessage msg attrs
    PlaceTokens _ (isTarget attrs -> True) Progress _ | attrs.flipped -> do
      victory <- getsGame ((.scenarioVictoryDisplay) . toAttrs . gameScenario)
      if any ((== toCardCode Locations.helmOfTheStormcaller) . toCardCode) victory
        then TakeTheShip <$> runMessage msg attrs
        else pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      aside <- getsGame ((.scenarioSetAside) . toAttrs . gameScenario)
      for_ [c | c <- aside, toCardCode c `elem` map toCardCode [Enemies.captainSahir, Enemies.naasiyah]] \c ->
        placeResourcesOutOfPlay c.id 1
      pure q
    _ -> TakeTheShip <$> runMessage msg attrs
