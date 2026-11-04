module LordOfTheRings.Scenario.Scenarios.AShadowOfThePast (aShadowOfThePast) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (HeroWithTrait), cardIs, cardMatches, getHero, select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype AShadowOfThePast = AShadowOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aShadowOfThePast :: Scenario
aShadowOfThePast = Scenario $ scenario AShadowOfThePast "a-shadow-of-the-past" "A Shadow of the Past"

{- | A Shadow of the Past, Hunted, The Ring and The Black Riders make up the encounter deck
(The Black Riders rules insert). Bucklebury Ferry is set aside for stage 1A, which has the
scenario do it as it gathers.

Campaign: Setup: Attach Mr. Underhill to the Ring-bearer. Add Gandalf's Delay to the
staging area. Shuffle Gildor Inglorion and The Ring Draws Them into the encounter deck.
Gildor Inglorion and The Ring Draws Them go in as the deck is gathered, to be shuffled at
step 2; Mr. Underhill and Gandalf's Delay at step 4, once the Ring-bearer is in play and
before the starting hands Gandalf's Delay shortens are drawn. Its resolution is chosen as
the players win (see 'winAShadowOfThePast').
-}
instance RunMessage AShadowOfThePast where
  runMessage msg (AShadowOfThePast attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.AShadowOfThePast, EncounterSet.Hunted, EncounterSet.TheRing, EncounterSet.TheBlackRiders]
      campaignCards <-
        if isJust attrs.campaign
          then traverse genEncounterCard [Objectives.gildorInglorionObjective, Treacheries.theRingDrawsThem]
          else pure []
      let (ferry, deck) = partition (cardMatches $ cardIs Locations.buckleburyFerry) cards
      putQuestIntoPlay =<< genEncounterCard Quests.threeIsCompany
      laterStages <- traverse genEncounterCard [Quests.aShortcutToMushrooms, Quests.escapeToBuckland]
      pure
        $ AShadowOfThePast
        $ attrs
        & encounterDeckL .~ deck <> campaignCards
        & setAsideL .~ ferry
        & questDeckL .~ laterStages
    DoStep 4 Setup | isJust attrs.campaign -> do
      ringBearers <- traverse getHero =<< select (HeroWithTrait RingBearer)
      for_ (take 1 ringBearers) \h -> do
        card <- genPlayerCard Attachments.mrUnderhill (Just h.owner)
        attachOutOfPlay h.owner card h.id
      addToStaging =<< genEncounterCard Objectives.gandalfsDelay
      AShadowOfThePast <$> runMessage msg attrs
    _ -> AShadowOfThePast <$> runMessage msg attrs
