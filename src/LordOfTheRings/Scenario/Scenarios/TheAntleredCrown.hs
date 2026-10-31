module LordOfTheRings.Scenario.Scenarios.TheAntleredCrown (theAntleredCrown) where

import Data.Aeson.Types (parseMaybe)
import Data.List (nubBy)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers
import LordOfTheRings.Source (Source, sourceTarget)

newtype TheAntleredCrown = TheAntleredCrown ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theAntleredCrown :: Scenario
theAntleredCrown = Scenario $ scenario TheAntleredCrown "the-antlered-crown" "The Antlered Crown"

{- | The Raven deck (rules insert): the enemies of Dunland Raiders and Dunland Warriors leave
the encounter deck for a deck of their own. A card revealed from it is staged as if
revealed from the encounter deck, its surge revealing from the encounter deck. An enemy
from it that leaves play goes to its own discard pile, which is shuffled back in when the
deck is empty.
-}
instance RunMessage TheAntleredCrown where
  runMessage msg (TheAntleredCrown attrs') = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheAntleredCrown, EncounterSet.DunlandRaiders, EncounterSet.DunlandWarriors]
      let (raven, encounterDeck) = partition isRavenCard cards
      deck <- shuffleM raven
      firstStage <- genEncounterCard Quests.battleForDunland
      laterStages <- traverse genEncounterCard [Quests.theRavenClan, Quests.theLastStage]
      putQuestIntoPlay firstStage
      pure
        $ TheAntleredCrown
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & metaL .~ toJSON (Meta deck [] (map (.id) deck))
    ScenarioSpecific "raven.draw" (parseMaybe parseJSON -> Just draw) -> do
      (deck, discards) <-
        if null meta.ravenDeck
          then (,[]) <$> shuffleM meta.ravenDiscard
          else pure (meta.ravenDeck, meta.ravenDiscard)
      case deck of
        card : rest -> do
          case draw of
            RavenReveal -> push $ RevealCard card
            RavenToStaging -> addToStaging card
            RavenEngaged pid -> putEnemyIntoPlayEngaged pid card
            RavenShadow eid -> push $ AttachShadowCard eid card
          pure $ withMeta meta {ravenDeck = rest, ravenDiscard = discards}
        [] -> pure $ TheAntleredCrown attrs
    -- each player in turn chooses an enemy of a title not yet chosen
    ScenarioSpecific "raven.choose" (parseMaybe parseJSON -> Just pid) -> do
      inPlay <- getsGame (map toCardCode . toList . (.entities.enemies))
      let choices = nubBy (\x y -> toCardCode x == toCardCode y) $ filter ((`notElem` inPlay) . toCardCode) meta.ravenDeck
      chooseOneM pid do
        questionLabeled "theAntleredCrown.chooseRavenEnemy"
        showing (map EncounterCard meta.ravenDeck)
        targets choices \card -> do
          push $ ScenarioSpecific "raven.remove" (toJSON card)
          addToStaging card
      pure $ TheAntleredCrown attrs
    ScenarioSpecific "raven.remove" (parseMaybe parseJSON -> Just card) ->
      pure $ withMeta meta {ravenDeck = filter (/= card) meta.ravenDeck}
    ScenarioSpecific "raven.shuffleDiscard" _ -> do
      deck <- shuffleM (meta.ravenDiscard <> meta.ravenDeck)
      pure $ withMeta meta {ravenDeck = deck, ravenDiscard = []}
    ScenarioSpecific "removeTimeCounters" (parseMaybe parseJSON -> Just (source :: Source, pid, n :: Int)) -> do
      timed <- filterM (fmap (> 0) . getTimeCounters . toTarget) =<< select AnyLocation
      chooseOrRunOneM pid do
        questionLabeledWith "theAntleredCrown.removeTimeCounter" [intVar "n" n]
        traverse_ causedBy (sourceTarget source)
        targets timed \l -> do
          removeTimeCounters source l 1
          removeTimeCountersFromLocations source pid (n - 1)
      pure $ TheAntleredCrown attrs
    _ -> TheAntleredCrown <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m = TheAntleredCrown $ attrs & metaL .~ toJSON m
    -- an enemy from the Raven deck discarded since the last message goes to the Raven
    -- deck's discard pile, wherever the game put it in the encounter discard pile
    attrs
      | any ((== EnemyType) . cdCardType . toCardDef) attrs'.encounterDiscard =
          let m = getMeta attrs'
           in case partition ((`elem` m.ravenCards) . (.id)) attrs'.encounterDiscard of
                ([], _) -> attrs'
                (raven, rest) -> attrs' & encounterDiscardL .~ rest & metaL .~ toJSON m {ravenDiscard = raven <> m.ravenDiscard}
      | otherwise = attrs'
