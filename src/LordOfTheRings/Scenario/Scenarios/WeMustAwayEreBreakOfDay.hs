module LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay (weMustAwayEreBreakOfDay) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Matcher (CardMatcher (..), cardIs, cardMatches)
import LordOfTheRings.Trait (Trait (Sack, Troll))

newtype WeMustAwayEreBreakOfDay = WeMustAwayEreBreakOfDay ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weMustAwayEreBreakOfDay :: Scenario
weMustAwayEreBreakOfDay = Scenario $ scenario WeMustAwayEreBreakOfDay "we-must-away-ere-break-of-day" "We Must Away, Ere Break of Day"

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
    _ -> do
      attrs' <- runMessage msg attrs
      let (treasures, discardPile) = partition trollTreasure attrs'.encounterDiscard
      traverse_ (push . PutObjectiveIntoPlay) treasures
      pure $ WeMustAwayEreBreakOfDay $ attrs' & encounterDiscardL .~ discardPile
   where
    trollTreasure c = toCardCode c `elem` map toCardCode [Objectives.trollPurse, Objectives.trollKey]
