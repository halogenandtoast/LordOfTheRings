module LordOfTheRings.Quest.Cards.BattleForDunland (battleForDunland) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (chooseRavenEnemy, removeTimeCounterFromEachLocation)

newtype BattleForDunland = BattleForDunland QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleForDunland :: QuestCard BattleForDunland
battleForDunland = quest BattleForDunland Cards.battleForDunland

-- | Forced: After the last time counter is removed from this stage, remove 1 time counter
-- from each location in play. Place 3 time counters on this stage.
instance HasAbilities BattleForDunland where
  getAbilities (BattleForDunland a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a | a.flipped]

-- | Setup: The first player takes control of Chief Turch. Set Raven Chief and Raven
-- Chief's Camp aside, out of play. Reveal 1 copy of Dunland Battlefield and make it the
-- active location. Reveal 1 copy of Raven War-camp and add it to the staging area. Create
-- the Raven deck (see insert) and set it next to the quest deck. Each player chooses 1
-- different enemy from the Raven deck and adds it to the staging area. Shuffle the
-- encounter deck and Raven deck.
-- The scenario made the Raven deck, shuffled, as it gathered the encounter deck.
instance RunMessage BattleForDunland where
  runMessage msg q@(BattleForDunland attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter ((== toCardCode def) . toCardCode) deck
          firstOf def f = for_ (take 1 $ copies def) \card -> do
            removeFromEncounterDeck card
            f card
      -- the objective ally comes under the first player's control as it is added
      firstOf Objectives.chiefTurchTheAntleredCrown addToStaging
      traverse_ setAside $ concatMap copies [Enemies.ravenChief, Locations.ravenChiefsCamp]
      firstOf Locations.dunlandBattlefield putIntoPlayAsActiveLocation
      firstOf Locations.ravenWarCamp addToStaging
      eachPlayer chooseRavenEnemy
      shuffleEncounterDeck
      pure $ BattleForDunland $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCounterFromEachLocation attrs
      placeTimeCounters attrs attrs 3
      pure q
    _ -> BattleForDunland <$> runMessage msg attrs
