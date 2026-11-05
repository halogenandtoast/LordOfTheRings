module LordOfTheRings.Scenario.Scenarios.FliesAndSpiders (fliesAndSpiders) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gamePlayerOrder)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (..), PlayerMatcher (..), getPlayerOrder, select)
import LordOfTheRings.Message.Lifted.Choose
import LordOfTheRings.Modifier (ModifierType (CannotBePoisoned))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype FliesAndSpiders = FliesAndSpiders ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fliesAndSpiders :: Scenario
fliesAndSpiders =
  Scenario
    $ scenario (FliesAndSpiders . (nightmareDeckL ?~ nightmare)) "flies-and-spiders" "Flies and Spiders"
 where
  -- https://hallofbeorn.com/LotR/Details/Flies-and-Spiders-Nightmare-FaSN
  nightmare =
    nightmareDeck
      Nightmare.fliesAndSpidersNightmare
      EncounterSet.FliesAndSpidersNightmare
      [ (Enemies.darkBats, 3)
      , (Locations.hiddenPath, 3)
      , (Treacheries.fightingAmongFriends, 2)
      , (Enemies.wickedSpider, 5)
      , (Enemies.fatSpider, 1)
      , (Locations.giantWeb, 1)
      ]

{- | Flies and Spiders and Wilderland make up the encounter deck (The Hobbit: On the
Doorstep rules insert). Stage 1A sets The Spider's Glade aside, out of play, until stage 4A
adds it to the staging area. The first player takes control of Bilbo Baggins as the game
sets up (see 'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage FliesAndSpiders where
  runMessage msg (FliesAndSpiders attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.FliesAndSpiders, EncounterSet.Wilderland]
      let (glade, deck) = partition ((== toCardCode Locations.theSpidersGlade) . toCardCode) cards
      firstStage <- genEncounterCard Quests.intoMirkwood
      laterStages <-
        traverse
          genEncounterCard
          [Quests.lostInTheDarkFliesAndSpiders, Quests.capturedBySpiders, Quests.battleWithTheSpiders]
      putQuestIntoPlay firstStage
      pure
        $ FliesAndSpiders
        $ attrs
        & encounterDeckL
        .~ deck
        & setAsideL
        .~ glade
        & questDeckL
        .~ laterStages
    DoStep 8 Setup | attrs.nightmare -> do
      players <- getPlayerOrder
      for_ players \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        able <- filterM (fmap (notElem CannotBePoisoned) . getModifiers) heroes
        chooseOrRunOneM pid do
          questionLabeled "fliesAndSpiders.givePoison"
          causedBy attrs
          targets able \h -> givePoison h 1
      FliesAndSpiders <$> runMessage msg attrs
    -- Won, the players have discovered the treasure card Bilbo's Magic Ring: each of them
    -- may add it to his deck in the scenarios that list its set (The Hobbit: On the
    -- Doorstep rules insert, "Treasure").
    ResolveCampaign | Just l <- attrs.campaign -> do
      seats <- getsGame (length . gamePlayerOrder)
      let ring seat =
            PoolCard
              { poolCardCode = toCardCode Attachments.bilbosMagicRing
              , poolCardSeat = Just seat
              , poolCardAttachedTo = Nothing
              }
          l' = foldl' (flip addToPool) l (map ring [0 .. seats - 1])
      FliesAndSpiders <$> runMessage msg (attrs & campaignL ?~ l')
    _ -> FliesAndSpiders <$> runMessage msg attrs
