{- | The Dream-chaser cycle's rules the scenario keeps (Grey Havens and Dream-chaser rules
inserts): the heading and the Sailing test, the Corsair deck and the Boarding keyword,
and preparing the fleet. A scenario runs them by falling through to its attrs.
-}
module LordOfTheRings.Scenario.Sailing (runSailing, isCorsairDeckCard, discardEncounterCard, fleet) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign (upgradedCode)
import LordOfTheRings.Card
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage ()
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.I18n (intVar)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Window (WindowType (CommittedToSailingTest, DiscardedFromEncounterDeck), mkAfter)

-- | A card of the Corsair deck: an enemy without the Ship trait.
isCorsairDeckCard :: EncounterCard -> Bool
isCorsairDeckCard card = cdCardType def == EnemyType && Ship `notMember` cdCardTraits def
 where
  def = toCardDef card

-- | The encounter card goes to the encounter discard pile, or, a card of the Corsair deck
-- while there is one, to the Corsair discard pile, and one of the Evil Creatures deck to
-- its own: an empty Evil Creatures deck takes it back at once, as its discard pile is
-- shuffled into it whenever it is empty (Journey Up the Anduin rules).
discardEncounterCard :: EncounterCard -> ScenarioAttrs -> ScenarioAttrs
discardEncounterCard card attrs
  | isJust attrs.corsairDeck && isCorsairDeckCard card = attrs & corsairDiscardL %~ (card :)
  | isEvilCreaturesCard attrs card =
      if attrs.evilCreaturesDeck == Just []
        then attrs & evilCreaturesDeckL ?~ [card]
        else attrs & evilCreaturesDiscardL %~ (card :)
  | otherwise = attrs & encounterDiscardL %~ (card :)

-- | The Ship objectives of The Dream-chaser's Fleet, the Dream-chaser first.
fleet :: [CardDef]
fleet = [Objectives.dreamChaser, Objectives.dawnStar, Objectives.narelenya, Objectives.silverWing]

runSailing :: Message -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
runSailing msg attrs = case msg of
  ShiftHeading _ shift -> pure $ attrs & headingL %~ fmap (shiftHeading shift)
  SailingTest -> do
    lead <- getLead
    shiftHeadingOffCourse GameSource
    push $ CommitToSailingTest lead []
    pure attrs
  -- the first player exhausts his characters to commit them, and the Dream-chaser whoever
  -- controls it
  CommitToSailingTest pid committed -> do
    own <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
    anywhere <- filterM (fmap (elem CommittableToAnySailingTest) . getModifiers) . map toTarget =<< select ReadyAlly
    candidates <- filterM (fmap (notElem CannotExhaust) . getModifiers) [c | c <- ordNub (own <> anywhere), c `notElem` committed]
    chooseOneM pid do
      questionLabeledWith "sailingTest.commit" [intVar "count" (length committed)]
      targets candidates \c -> do
        exhaust c
        push $ CommitToSailingTest pid (committed <> [c])
      labeled "sailingTest.done" $ push $ ResolveSailingTest committed
    pure attrs
  ResolveSailingTest committed -> do
    controllers <- for committed \c -> (,[c]) <$> characterController c
    push $ CheckWindows [mkAfter $ CommittedToSailingTest p cs | (Just p, cs) <- Map.toList (Map.fromListWith (flip (<>)) controllers)]
    push $ DoStep 1 msg
    pure attrs
  -- as many cards as characters committed are looked at, the encounter discard pile
  -- shuffled in beneath the deck should it run short
  DoStep 1 (ResolveSailingTest committed) -> do
    counts <- for committed $ fmap sailingCharacters . getModifiers
    gameMods <- getModifiers GameTarget
    let n = sum counts + sum [k | SailingTestAdditionalCards k <- gameMods]
    (deck, discarded) <-
      if length attrs.encounterDeck < n
        then (\d -> (attrs.encounterDeck <> d, [])) <$> shuffleM attrs.encounterDiscard
        else pure (attrs.encounterDeck, attrs.encounterDiscard)
    let (top, rest) = splitAt n deck
        found = sum $ map (cdSailingIcons . toCardDef) top
        canceled = if CancelFirstSailingIcon `elem` gameMods then min 1 found else 0
    push $ SailingTestResult top found canceled
    for_ top \c -> push $ ResolveSailingDiscard c committed
    push $ SailingTestEnded top committed
    pure $ attrs & encounterDeckL .~ rest & encounterDiscardL .~ discarded
  -- each success icon not canceled shifts the heading on-course, then the cards are
  -- discarded
  SailingTestResult top found canceled -> do
    lead <- getLead
    chooseOneM lead do
      questionLabeledWith "sailingTest.result" [intVar "count" (length top), intVar "found" found, intVar "canceled" canceled]
      showing (map EncounterCard top)
      labeled "continue" do
        replicateM_ (found - canceled) $ shiftHeadingOnCourse GameSource
        push $ AddToEncounterDiscard top
        push $ CheckWindows [mkAfter $ DiscardedFromEncounterDeck c.id | c <- top]
    pure attrs
  PrepareCorsairDeck -> do
    let (corsairs, rest) = partition isCorsairDeckCard attrs.encounterDeck
    deck <- shuffleM corsairs
    pure $ attrs & encounterDeckL .~ rest & corsairDeckL ?~ deck & corsairDiscardL .~ []
  RevealFromCorsairDeck source pid -> fromCorsairDeck source pid False
  RevealFromCorsairDeckWithShadow source pid -> fromCorsairDeck source pid True
  ResolveBoarding pid eid -> do
    x <- getBoarding eid
    replicateM_ x $ push $ RevealFromCorsairDeck (EnemySource eid) pid
    pure attrs
  -- the heading starts on-course; a lone player takes the Dream-chaser and one other
  PrepareFleet -> do
    players <- getRemainingPlayers
    -- in the Dream-chaser campaign, a ship's Upgraded side once XP has upgraded it
    ships <- traverse genEncounterCard [maybe def (\l -> toCardDef $ upgradedCode l (toCardCode def)) attrs.campaign | def <- fleet]
    case ships of
      dreamChaser : others -> case players of
        [solo] -> do
          push $ PutObjectiveAllyIntoPlayUnder solo dreamChaser
          push $ ChooseFleetShip [solo] others
        _ -> do
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled "fleet.dreamChaser"
            showing [EncounterCard dreamChaser]
            targets players \p -> do
              push $ PutObjectiveAllyIntoPlayUnder p dreamChaser
              push $ ChooseFleetShip (filter (/= p) players) others
      [] -> pure ()
    pure $ attrs & headingL ?~ Sunny
  ChooseFleetShip (pid : rest) ships -> do
    chooseOrRunOneM pid do
      questionLabeled "fleet.chooseShip"
      showing (map EncounterCard ships)
      targets ships \s -> do
        push $ PutObjectiveAllyIntoPlayUnder pid s
        push $ ChooseFleetShip rest (filter (/= s) ships)
    pure attrs
  _ -> pure attrs
 where
  -- the top card is put into play engaged with the player, the Corsair discard pile
  -- shuffled back in first once the deck is empty; with a shadow card, as Ramming Ship's
  fromCorsairDeck source pid shadowed = case attrs.corsairDeck of
    Nothing -> pure attrs
    Just deck -> do
      (deck', discarded) <- if null deck then (,[]) <$> shuffleM attrs.corsairDiscard else pure (deck, attrs.corsairDiscard)
      case deck' of
        [] -> pure attrs
        top : rest -> do
          push $ RevealCardEngaged pid top
          when shadowed $ push $ DealShadowCardToCard top.id
          push $ RevealedFromCorsairDeck source pid top
          pure $ attrs & corsairDeckL ?~ rest & corsairDiscardL .~ discarded
