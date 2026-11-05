module LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship (breakingOfTheFellowship) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (Captives), CampaignLogUpdate (AddFallenHero, AddToPool, Record), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.Types (enemyOwner)
import LordOfTheRings.Game.Base (gameEntities, gameRevealer)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Hero.Types (heroGuardedBy)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (isFrodosChoice, revealEncounterCardAt)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Window (Window (..), WindowType (DiscardedFromEncounterDeck))
import LordOfTheRings.Timing (Timing (After))

newtype BreakingOfTheFellowship = BreakingOfTheFellowship ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

breakingOfTheFellowship :: Scenario
breakingOfTheFellowship = Scenario $ scenario BreakingOfTheFellowship "breaking-of-the-fellowship" "Breaking of the Fellowship"

-- | The Ring-bearer, removed from the game by stage 2A, waits as Frodo's Choice's captive:
-- the cards attached to him are out of the game with him, their text not there to use.
instance HasModifiersFor BreakingOfTheFellowship where
  getModifiersFor (BreakingOfTheFellowship a) = do
    heroes <- getsGame (toList . (.entities.heroes))
    attached <- concatMapM (getAttachmentsOn . toTarget) [h | h <- heroes, isJust (heroGuardedBy $ toAttrs h)]
    for_ attached \t -> modified a t [Blank]

{- | The encounter deck is built from the Breaking of the Fellowship encounter set (The Road
Darkens rules insert). Stage 1A sets Frodo's Choice, Seat of Seeing and Parth Galen aside,
and adds The Argonath and Sarn Gebir to the staging area: they are set aside here, as the
deck is gathered, for the stage to stage them.

Stage 2B has each player create his own staging area: see
'LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers'.
-}
instance RunMessage BreakingOfTheFellowship where
  runMessage msg (BreakingOfTheFellowship attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.BreakingOfTheFellowship]
      let aside c =
            any
              (`cardMatches` c)
              [cardIs Objectives.frodosChoice, cardIs Locations.seatOfSeeing, cardIs Locations.parthGalen, cardIs Locations.theArgonath, cardIs Locations.sarnGebir]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.theGreatRiver
      laterStages <-
        traverse
          genEncounterCard
          [Quests.theCompanyDivided, Quests.searchingTheWoods, Quests.guardTheHobbits, Quests.theSeatOfAmonHen, Quests.orcHunting, Quests.theRingBearerSetsOut]
      putQuestIntoPlay firstStage
      pure
        $ BreakingOfTheFellowship
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Starting with the first player, each player chooses 1 of the
    -- following boon cards: Phial of Galadriel, Three Golden Hairs, Lórien Rope, or
    -- Leaf-wrapped Lembas, and attaches it to a hero he controls. The chosen boon cards have
    -- been earned by the players. Shuffle Followed by Night and Ill Fate into the encounter
    -- deck.
    DoStep 9 Setup | isJust attrs.campaign -> do
      players <- getRemainingPlayers
      push $ ScenarioSpecific "breakingOfTheFellowship.gift" (toJSON (players, map toCardCode gifts))
      burdens <- traverse genEncounterCard [Treacheries.followedByNightTheRoadDarkens, Treacheries.illFate]
      deck <- shuffleM (burdens <> attrs.encounterDeck)
      BreakingOfTheFellowship <$> runMessage msg (attrs & encounterDeckL .~ deck)
    ScenarioSpecific "breakingOfTheFellowship.gift" (parseMaybe parseJSON -> Just (players :: [PlayerId], left :: [CardCode])) -> do
      case players of
        p : ps | notNull left -> do
          heroes <- select (HeroControlledBy $ PlayerWithId p)
          cards <- traverse (\code -> genPlayerCard (toCardDef code) (Just p)) left
          seat <- seatOf p
          chooseOrRunOneM p do
            questionLabeled "breakingOfTheFellowship.chooseGift"
            showing (map PlayerCard cards)
            for_ cards \c -> targeting (CardIdTarget c.id) do
              chooseOrRunOneM p do
                questionLabeled "breakingOfTheFellowship.attachGift"
                showing [PlayerCard c]
                targets heroes \h -> push $ PlayAttachmentOn p c (toTarget h)
              updateCampaignLog $ AddToPool $ PoolCard (toCardCode c) seat Nothing
              push $ ScenarioSpecific "breakingOfTheFellowship.gift" (toJSON (ps, filter (/= toCardCode c) left))
        _ -> pure ()
      pure $ BreakingOfTheFellowship attrs
    -- Stage 2B's Forced effect: shuffle the Seat of Seeing and Frodo's Choice into the
    -- encounter discard pile, and place the pile on the bottom of the encounter deck. Then,
    -- starting with the first player, each player advances to a different stage 3A of his
    -- choice, in his own staging area.
    ScenarioSpecific "breakingOfTheFellowship.divide" _ -> do
      let shuffledIn c = any (`cardMatches` c) [cardIs Locations.seatOfSeeing, cardIs Objectives.frodosChoice]
          (joining, setAside') = partition shuffledIn attrs.scenarioSetAside
      pile <- shuffleM (joining <> attrs.encounterDiscard)
      players <- getRemainingPlayers
      push $ ScenarioSpecific "breakingOfTheFellowship.chooseStage" (toJSON (players, [] :: [(PlayerId, EncounterCard)]))
      pure
        $ BreakingOfTheFellowship
        $ attrs
        & setAsideL .~ setAside'
        & encounterDiscardL .~ []
        & encounterDeckL %~ (<> pile)
    ScenarioSpecific "breakingOfTheFellowship.chooseStage" (parseMaybe parseJSON -> Just (players :: [PlayerId], chosen :: [(PlayerId, EncounterCard)])) -> do
      let stages = [c | c <- attrs.questDeck, cdStage (toCardDef c) == Just 3, c `notElem` map snd chosen]
      case players of
        p : ps | notNull stages -> chooseOrRunOneM p do
          questionLabeled "breakingOfTheFellowship.chooseStage"
          showing (map EncounterCard stages)
          for_ stages \c -> targeting (CardIdTarget c.id) do
            push $ ScenarioSpecific "breakingOfTheFellowship.chooseStage" (toJSON (ps, chosen <> [(p, c)]))
        _ -> do
          -- each player creates his own staging area, the cards of this one to move to them
          separateStagingAreas (map (pure . fst) chosen)
          es <- getsGame gameEntities
          let staged =
                [toTarget e | e <- toList es.enemies, e.placement == StagingArea]
                  <> [toTarget l | l <- toList es.locations, l.placement == StagingArea]
                  <> [toTarget t | t <- toList es.treacheries, t.placement == StagingArea]
                  <> [toTarget o | o <- toList es.objectives, o.placement == StagingArea]
          push $ ScenarioSpecific "breakingOfTheFellowship.distribute" (toJSON (map fst chosen, staged, chosen))
      pure $ BreakingOfTheFellowship attrs
    -- In player order, each player moves 1 encounter card from this staging area to his:
    -- the first player's own is this one, so his stay where they are. When there are no
    -- encounter cards left at this stage, discard it.
    ScenarioSpecific "breakingOfTheFellowship.distribute" (parseMaybe parseJSON -> Just (order :: [PlayerId], left :: [Target], chosen :: [(PlayerId, EncounterCard)])) -> do
      lead <- getLead
      case order of
        p : rest | notNull left -> chooseOrRunOneM p do
          questionLabeled "breakingOfTheFellowship.moveCard"
          targets left \t -> do
            when (p /= lead) $ placeInStagingAreaOf p t
            push $ ScenarioSpecific "breakingOfTheFellowship.distribute" (toJSON (rest <> [p], filter (/= t) left, chosen))
        _ -> do
          for_ chosen \(p, c) -> do
            focusAreaOf p
            putQuestIntoPlay c
          for_ [c | c <- attrs.questDeck, cdStage (toCardDef c) == Just 3, c `notElem` map snd chosen] (push . RemoveFromQuestDeck)
          focusAreaOf lead
          divided <- getsGame (filter ((== toCardCode Quests.theCompanyDivided) . toCardCode) . toList . (.entities.quests))
          for_ divided \q -> push $ LeaveQuestStage q.id
          -- each stage 3A: "Forced: After stage 2B is discarded, if the total threat of
          -- encounter cards in this staging area is less than 4, reveal 1 encounter card."
          for_ chosen \(p, c) -> do
            focusAreaOf p
            push $ ScenarioSpecific "breakingOfTheFellowship.stageThree" (toJSON p)
            flipStage c
          focusAreaOf lead
      pure $ BreakingOfTheFellowship attrs
    ScenarioSpecific "breakingOfTheFellowship.stageThree" (parseMaybe parseJSON -> Just (p :: PlayerId)) -> do
      threat <- getStagingThreat
      when (threat < 4) $ revealEncounterCardAt p
      pure $ BreakingOfTheFellowship attrs
    -- Frodo's Choice: "If Frodo's Choice is discarded, reveal it instead", at the stage
    -- whose effect discarded it, its player its "you".
    AddToEncounterDiscard cards | any isFrodosChoice cards -> do
      let (choices, rest) = partition isFrodosChoice cards
      traverse_ revealInstead choices
      BreakingOfTheFellowship <$> runMessage (AddToEncounterDiscard rest) attrs
    CheckWindows ws
      | choices@(_ : _) <- [c | Window After (DiscardedFromEncounterDeck cid) <- ws, c <- attrs.encounterDiscard, c.id == cid, isFrodosChoice c] -> do
          traverse_ revealInstead choices
          BreakingOfTheFellowship <$> runMessage msg (attrs & encounterDiscardL %~ filter (`notElem` choices))
    -- Resolution: the first player chooses either Followed by Night or Ill Fate to be added
    -- to the Campaign Pool (the chosen burden card has been earned by the players), and
    -- each player chooses 1 hero he controls (other than the Ring-bearer or Aragorn) to be
    -- taken captive, whose name is recorded in the Campaign Log. If Fallen into Evil is
    -- attached to a hero, add that hero to the list of Fallen Heroes in the Campaign Log:
    -- the hero it turned into an enemy.
    ResolveCampaign | Just l <- attrs.campaign -> do
      lead <- getLead
      burdens <- traverse genEncounterCard [Treacheries.followedByNightTheRoadDarkens, Treacheries.illFate]
      chooseOneM lead do
        questionLabeled "breakingOfTheFellowship.chooseBurden"
        showing (map EncounterCard burdens)
        for_ burdens \c -> targeting (CardIdTarget c.id) do
          updateCampaignLog $ AddToPool $ PoolCard (toCardCode c) Nothing Nothing
      players <- getRemainingPlayers
      push $ ScenarioSpecific "breakingOfTheFellowship.captive" (toJSON (players, [] :: [CardCode]))
      es <- getsGame gameEntities
      let fallen =
            [ toCardCode e
            | t <- toList es.treacheries
            , toCardCode t == toCardCode Treacheries.fallenIntoEvil
            , AttachedTo (EnemyTarget eid) <- [t.placement]
            , Just e <- [lookup eid es.enemies]
            , isJust (enemyOwner $ toAttrs e)
            , cdCardType (toCardDef e) == HeroType
            ]
      BreakingOfTheFellowship <$> runMessage msg (attrs {scenarioCampaign = Just $ foldl' (flip $ updateLog . AddFallenHero) l fallen})
    ScenarioSpecific "breakingOfTheFellowship.captive" (parseMaybe parseJSON -> Just (players :: [PlayerId], captives :: [CardCode])) -> do
      case players of
        p : ps -> do
          bearers <- select (HeroWithTrait RingBearer)
          heroes <- select (HeroControlledBy (PlayerWithId p))
          eligible <- flip filterM heroes \h -> do
            captive <- getHero h
            pure $ h `notElem` bearers && title (toCardDef captive) /= "Aragorn"
          if null eligible
            then push $ ScenarioSpecific "breakingOfTheFellowship.captive" (toJSON (ps, captives))
            else chooseOrRunOneM p do
              questionLabeled "breakingOfTheFellowship.chooseCaptive"
              targets eligible \h -> do
                captive <- getHero h
                push $ ScenarioSpecific "breakingOfTheFellowship.captive" (toJSON (ps, captives <> [toCardCode captive]))
        [] -> updateCampaignLog $ Record Captives captives
      pure $ BreakingOfTheFellowship attrs
    _ -> BreakingOfTheFellowship <$> runMessage msg attrs
   where
    gifts = [Attachments.phialOfGaladriel, Attachments.threeGoldenHairs, Attachments.lorienRope, Attachments.leafWrappedLembas]
    title def = let Name t _ = cdName def in t
    revealInstead :: ReverseQueue m => EncounterCard -> m ()
    revealInstead c = do
      lead <- getStageLead
      revealer <- getsGame gameRevealer
      push $ SetRevealer (Just lead)
      push $ RevealCard c
      push $ SetRevealer revealer
