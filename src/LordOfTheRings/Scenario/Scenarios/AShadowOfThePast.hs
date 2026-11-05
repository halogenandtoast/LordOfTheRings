module LordOfTheRings.Scenario.Scenarios.AShadowOfThePast (aShadowOfThePast) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (
  EnemyMatcher (EnemyWithCardCode),
  HeroMatcher (HeroWithTrait),
  cardIs,
  cardMatches,
  getHero,
  getLead,
  select,
  selectAny,
 )
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype AShadowOfThePast = AShadowOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aShadowOfThePast :: Scenario
aShadowOfThePast =
  Scenario
    $ scenario
      (AShadowOfThePast . (nightmareDeckL ?~ nightmare))
      "a-shadow-of-the-past"
      "A Shadow of the Past"
 where
  -- https://hallofbeorn.com/LotR?EncounterSet=A+Shadow+of+the+Past+Nightmare
  nightmare =
    nightmareDeck
      Nightmare.aShadowOfThePastNightmare
      EncounterSet.AShadowOfThePastNightmare
      [ (Enemies.evilCrowAShadowOfThePast, 3)
      , (Locations.woodyEnd, 1)
      , (Locations.bamfurlong, 1)
      , (Locations.stockBrook, 1)
      , (Locations.greenHillCountry, 3)
      , (Locations.pathlessCountryHunted, 3)
      , (Treacheries.rodeLikeAGale, 2)
      ]

{- | A Shadow of the Past, Hunted, The Ring and The Black Riders make up the encounter deck
(The Black Riders rules insert). Bucklebury Ferry is set aside for stage 1A, which has the
scenario do it as it gathers.

Campaign: Setup: Attach Mr. Underhill to the Ring-bearer. Add Gandalf's Delay to the
staging area. Shuffle Gildor Inglorion and The Ring Draws Them into the encounter deck.
Gildor Inglorion and The Ring Draws Them go in as the deck is gathered, to be shuffled at
step 2; Mr. Underhill and Gandalf's Delay at step 4, once the Ring-bearer is in play and
before the starting hands Gandalf's Delay shortens are drawn.

Resolution: Choose Gandalf's Delay or The Ring Draws Them and add it to the Campaign Pool.
Then, choose Gildor Inglorion or Mr. Underhill and add it to the Campaign Pool. The chosen
cards have been earned by the players. The first player chooses for the group. Mr.
Underhill, Permanent, is recorded on the Ring-bearer (The Black Riders rules insert,
"Permanent").
-}
instance RunMessage AShadowOfThePast where
  runMessage msg (AShadowOfThePast attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.AShadowOfThePast
          , EncounterSet.Hunted
          , EncounterSet.TheRing
          , EncounterSet.TheBlackRiders
          ]
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
        & encounterDeckL
        .~ deck
        <> campaignCards
        & setAsideL
        .~ ferry
        & questDeckL
        .~ laterStages
    Flip target | attrs.nightmare -> do
      entering <-
        getsGame
          ( any (\q -> isTarget q target && toCardCode q == toCardCode Quests.escapeToBuckland && not q.flipped)
              . toList
              . (.entities.quests)
          )
      when entering do
        present <- selectAny (EnemyWithCardCode $ toCardCode Enemies.khamulTheEasterlingNightmare)
        unless present
          $ searchEncounterCardsAndVictoryDisplay (cardIs Enemies.khamulTheEasterlingNightmare) addToStaging
      AShadowOfThePast <$> runMessage msg attrs
    DoStep 4 Setup | isJust attrs.campaign -> do
      ringBearers <- traverse getHero =<< select (HeroWithTrait RingBearer)
      for_ (take 1 ringBearers) \h -> do
        card <- genPlayerCard Attachments.mrUnderhill (Just h.owner)
        attachOutOfPlay h.owner card h.id
      addToStaging =<< genEncounterCard Objectives.gandalfsDelay
      AShadowOfThePast <$> runMessage msg attrs
    ResolveCampaign | isJust attrs.campaign -> do
      lead <- getLead
      ringBearer <- traverse getHero . headMay =<< select (HeroWithTrait RingBearer)
      let earn def on = updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) Nothing on
      chooseOneM lead do
        questionLabeled "aShadowOfThePast.chooseBurden"
        labeled "aShadowOfThePast.gandalfsDelay" $ earn Objectives.gandalfsDelay Nothing
        labeled "aShadowOfThePast.theRingDrawsThem" $ earn Treacheries.theRingDrawsThem Nothing
      chooseOneM lead do
        questionLabeled "aShadowOfThePast.chooseBoon"
        labeled "aShadowOfThePast.gildorInglorion" $ earn Objectives.gildorInglorionObjective Nothing
        labeled "aShadowOfThePast.mrUnderhill" $ earn Attachments.mrUnderhill (toCardCode <$> ringBearer)
      AShadowOfThePast <$> runMessage msg attrs
    _ -> AShadowOfThePast <$> runMessage msg attrs
