{- | What The Road to Isengard's cards share: the Wizardry effects of its locations, and the
progress stage 3B has taken this phase.

Wizardry is a timing trigger: when a location with Wizardry leaves play as an explored
location, its Wizardry effect triggers, even if it is not the active location (MEC45
rules, "Wizardry"). Machines of Isengard triggers the active location's, and Saruman has
the defending player resolve one dealt to him as his shadow card as if he were the only
player in the game: the effect is read off the location's card wherever it is.
-}
module LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (
  hasWizardry,
  wizardry,
  questProgressThisPhase,
  questProgressCanceled,
  recordQuestProgress,
  cancelQuestProgress,
  progressOf,
  canceledOf,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardCode, CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (ScenarioSpecific))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioMeta)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target (toTarget)

-- | The locations printed with a Wizardry effect.
hasWizardry :: CardCode -> Bool
hasWizardry = (`elem` map toCardCode [Locations.ringWallOfIsengard, Locations.undergroundArmoury, Locations.steamingVent, Locations.plainOfIsengard, Locations.openPit, Locations.poisonousShaft, Locations.fortressOfIsengard])

{- | The location card's Wizardry effect, resolved for the players given, as if they were
the only players in the game: each of them when it triggers, the defending player alone
when Saruman is dealt it. The first of them makes the choices no one else is named for.

* Ring-wall of Isengard: Each player discards 1 random card from his hand.
* Underground Armoury: Each player exhausts a character he controls.
* Steaming Vent: Assign X damage among characters in play. X is 1 more than the number of
  players in the game.
* Plain of Isengard: Each player discards 1 ally he controls.
* Open Pit: Each player raises his threat by 2.
* Poisonous Shaft: Each player discards a damaged character he controls.
* Fortress of Isengard: Return the topmost enemy in the encounter discard pile to staging.
-}
wizardry :: (ReverseQueue m, Sourceable source) => source -> CardCode -> [PlayerId] -> m ()
wizardry source code players
  | code == toCardCode Locations.ringWallOfIsengard = for_ players \p -> discardRandomFromHand p 1
  | code == toCardCode Locations.undergroundArmoury = for_ players \p -> chooseAndExhaust source p 1
  | code == toCardCode Locations.steamingVent = for_ (take 1 players) \p -> do
      characters <- concatMapM charactersControlledBy players
      when (notNull characters) $ dealDamageDividedAmong source p characters (length players + 1)
  | code == toCardCode Locations.plainOfIsengard = for_ players \p -> do
      allies <- map toTarget <$> select (AllyControlledBy $ PlayerWithId p)
      when (notNull allies) $ chooseAndDiscard source p allies
  | code == toCardCode Locations.openPit = for_ players \p -> encounterRaisesThreat p 2
  | code == toCardCode Locations.poisonousShaft = for_ players \p -> do
      damaged <- filterM (fmap (> 0) . getDamage) =<< charactersControlledBy p
      chooseAndDiscard source p damaged
  | code == toCardCode Locations.fortressOfIsengard = do
      discards <- getEncounterDiscard
      for_ (find (cardMatches $ CardWithType EnemyType) discards) addToStagingFromDiscard
  | otherwise = pure ()

-- | The progress placed on the quest this phase, kept in the scenario's meta.
progressOf :: Value -> Int
progressOf = fromMaybe 0 . parseMaybe (withObject "meta" (.: "questProgress"))

-- | Whether the progress placed on the quest this phase is canceled, as Orthanc's
-- exploring has it at stage 3B.
canceledOf :: Value -> Bool
canceledOf = fromMaybe False . parseMaybe (withObject "meta" (.: "questProgressCanceled"))

questProgressThisPhase :: HasGame m => m Int
questProgressThisPhase = getsGame (progressOf . scenarioMeta . toAttrs . gameScenario)

questProgressCanceled :: HasGame m => m Bool
questProgressCanceled = getsGame (canceledOf . scenarioMeta . toAttrs . gameScenario)

recordQuestProgress :: ReverseQueue m => Int -> m ()
recordQuestProgress = push . ScenarioSpecific "roadToIsengard.questProgress" . toJSON

cancelQuestProgress :: ReverseQueue m => m ()
cancelQuestProgress = push $ ScenarioSpecific "roadToIsengard.cancelQuestProgress" Null
