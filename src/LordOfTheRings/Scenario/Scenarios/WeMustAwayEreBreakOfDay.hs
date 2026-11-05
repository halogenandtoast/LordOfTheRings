module LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay (weMustAwayEreBreakOfDay) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Modifier (ModifierType (GainKeyword))
import LordOfTheRings.Source (isQuestSource)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Matcher (CardMatcher (..), cardIs, cardMatches)
import LordOfTheRings.Trait (Trait (Sack, Troll))

newtype WeMustAwayEreBreakOfDay = WeMustAwayEreBreakOfDay ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

weMustAwayEreBreakOfDay :: Scenario
weMustAwayEreBreakOfDay = Scenario $ scenario (WeMustAwayEreBreakOfDay . (nightmareDeckL ?~ nightmare)) "we-must-away-ere-break-of-day" "We Must Away, Ere Break of Day"
 where
  nightmare =
    nightmareDeck
      Nightmare.weMustAwayEreBreakOfDayNightmare
      EncounterSet.WeMustAwayEreBreakOfDayNightmare
      [ (Enemies.aSuspiciousCrow, 3)
      , (Locations.loneLands, 1)
      , (Locations.drearyHills, 1)
      , (Locations.hobbitLands, 2)
      , (Treacheries.aNicePickle, 2)
      ]

-- | Nightmare setup card: Troll Key and Troll Purse gain surge.
-- https://hallofbeorn.com/LotR/Details/We-Must-Away-Ere-Break-of-Day-Nightmare-WMAEBoDN Objective revelations
-- resolve surge by card ID, after the objective has entered play.
instance HasModifiersFor WeMustAwayEreBreakOfDay where
  getModifiersFor (WeMustAwayEreBreakOfDay a) = when a.nightmare do
    treasures <- getsGame (filter trollTreasure . toList . (.entities.objectives))
    for_ treasures \o -> do
      modified a o [GainKeyword Surge]
      modified a (CardIdTarget (toAttrs o).cardId) [GainKeyword Surge]

trollTreasure :: HasCardCode c => c -> Bool
trollTreasure c = toCardCode c `elem` map toCardCode [Objectives.trollPurse, Objectives.trollKey]

{- | The We Must Away, Ere Break of Day and Western Lands encounter sets make up the
encounter deck. Stage 1A's setup takes the Sack cards out into the sack deck and sets the
Trolls and Troll Cave aside: done here as the deck is gathered, before it is shuffled.

Troll Purse and Troll Key: "If discarded, add it to the staging area." However one goes to
the encounter discard pile, from play or from the deck, it comes into play in the staging
area instead, to wait there for a Troll (players reporting the later printing's text over
Caleb's earlier ruling that they were discarded, FFG forum).
-}
instance RunMessage WeMustAwayEreBreakOfDay where
  runMessage msg (WeMustAwayEreBreakOfDay attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.WeMustAwayEreBreakOfDay, EncounterSet.WesternLands]
      let isSack = cardMatches (CardWithTrait Sack)
          aside c = cardMatches (CardMatchAny [CardWithTrait Troll, cardIs Locations.trollCave]) c
          (sacks, rest) = partition isSack cards
          (setAside', deck) = partition aside rest
      sackDeck <- shuffleM sacks
      firstStage <- genEncounterCard Quests.anUnexpectedParty
      laterStages <- traverse genEncounterCard [Quests.roastMutton, Quests.dawnTakeYouAll]
      putQuestIntoPlay firstStage
      pure
        $ WeMustAwayEreBreakOfDay
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & sackDeckL .~ sackDeck
        & questDeckL .~ laterStages
    -- The setup card explicitly requires stage 2's discards to resolve one at a time.
    -- A quest's discard can trigger this even though the treachery is out of play.
    DiscardedTopOfEncounterDeck source _ cards
      | attrs.nightmare
      , isQuestSource source
      , any ((== toCardCode Treacheries.nightWanesSlow) . toCardCode) cards -> do
          endCurrentPhase
          pure $ WeMustAwayEreBreakOfDay attrs
    _ -> do
      attrs' <- runMessage msg attrs
      let (treasures, discardPile) = partition trollTreasure attrs'.encounterDiscard
      traverse_ (push . PutObjectiveIntoPlay) treasures
      pure $ WeMustAwayEreBreakOfDay $ attrs' & encounterDiscardL .~ discardPile
