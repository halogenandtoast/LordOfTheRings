module LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim (dungeonsDeepAndCavernsDim) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getLead, getPlayer, getPlayerOrder)
import LordOfTheRings.Message.Lifted.Choose
import LordOfTheRings.Modifier (ModifierType (AnsweringRiddle))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers
import LordOfTheRings.Source (Source (CardIdSource, ScenarioSource))
import LordOfTheRings.Token (Token (Progress))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype DungeonsDeepAndCavernsDim = DungeonsDeepAndCavernsDim ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonsDeepAndCavernsDim :: Scenario
dungeonsDeepAndCavernsDim = Scenario $ scenario DungeonsDeepAndCavernsDim "dungeons-deep-and-caverns-dim" "Dungeons Deep and Caverns Dim"

-- | Once the items of a riddle are named and its cards discarded, the players may take
-- actions, as Bilbo's Magic Ring's first, before its matches are counted.
instance HasModifiersFor DungeonsDeepAndCavernsDim where
  getModifiersFor (DungeonsDeepAndCavernsDim a) =
    for_ (getMeta a).riddle \r -> when (null r.riddleToName) $ modified a GameTarget [AnsweringRiddle]

{- | The Dungeons Deep and Caverns Dim and Misty Mountain Goblins encounter sets make up the
encounter deck.

Riddles: when an encounter card with a riddle is revealed from the encounter deck, the
first player chooses between resolving it normally and answering its riddle; answered, it
is placed in the encounter discard pile once the riddle is (The Hobbit: Over Hill and
Under Hill rules insert, "Riddles"). It Likes Riddles? and What's In My Pocket? have the
first player answer theirs as they resolve, so they are revealed as any other card.

A riddle is answered in steps: the first player names each item, shuffles his deck and
discards the top X cards, the players may take actions (FAQ: "Players can take actions
after step 2, before step 3"), then 1 progress is placed on stage 2 for each card discarded
that matches every item named.
-}
instance RunMessage DungeonsDeepAndCavernsDim where
  runMessage msg (DungeonsDeepAndCavernsDim attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.DungeonsDeepAndCavernsDim, EncounterSet.MistyMountainGoblins]
      firstStage <- genEncounterCard Quests.outOfTheFryingPan
      laterStages <- traverse genEncounterCard [Quests.riddlesInTheDark, Quests.intoTheFire]
      putQuestIntoPlay firstStage
      pure $ DungeonsDeepAndCavernsDim $ attrs & encounterDeckL .~ cards & questDeckL .~ laterStages
    RevealEncounterCard | card : rest <- attrs.encounterDeck, hasRiddle card, not (answeredAsItResolves card) -> do
      ignored <- riddlesIgnored
      if ignored
        then DungeonsDeepAndCavernsDim <$> runMessage msg attrs
        else do
          lead <- getLead
          chooseOneM lead do
            questionLabeledWith "riddle.choose" (cardVar "card" card)
            showing [EncounterCard card]
            labeled "riddle.resolve" $ push (RevealCard card)
            labeled "riddle.answer" $ answerRiddle (CardIdSource card.id) True card
          pure $ DungeonsDeepAndCavernsDim $ attrs & encounterDeckL .~ rest
    ScenarioSpecific "riddle.answer" (parseMaybe parseJSON -> Just (source :: Source, fromReveal, card, Riddle items n)) -> do
      lead <- getLead
      let state = RiddleState card source fromReveal lead items [] n []
      nameNext state
      pure $ withRiddle (Just state)
    ScenarioSpecific "riddle.name" (parseMaybe parseJSON -> Just named) | Just r <- riddle -> do
      let state = r {riddleToName = drop 1 r.riddleToName, riddleNamed = r.riddleNamed <> [named]}
      nameNext state
      pure $ withRiddle (Just state)
    ScenarioSpecific "riddle.discard" (parseMaybe parseJSON -> Just (n :: Int)) | Just r <- riddle -> do
      deck <- (.deck) <$> getPlayer r.riddleAnswerer
      let discarded = take n deck
      discardTopOfDeck r.riddleAnswerer n
      pure $ withRiddle (Just r {riddleDiscarded = r.riddleDiscarded <> map toCardCode discarded})
    ScenarioSpecific "riddle.resolve" _ | Just r <- riddle -> do
      let matches = length [d | d <- map toCardDef r.riddleDiscarded, all (matchesNamed d) r.riddleNamed]
      stage <- getRiddleStage
      for_ stage \q -> when (matches > 0) $ placeTokens ScenarioSource q Progress matches
      when r.riddleFromReveal $ push $ AddToEncounterDiscard [r.riddleCard]
      push $ ScenarioSpecific "riddle.answered" $ toJSON (r.riddleSource, matches)
      pure $ withRiddle Nothing
    _ -> DungeonsDeepAndCavernsDim <$> runMessage msg attrs
   where
    riddle = (getMeta attrs).riddle
    withRiddle r = DungeonsDeepAndCavernsDim $ attrs & metaL .~ toJSON (Meta r)
    answeredAsItResolves card = toCardCode card `elem` map toCardCode [Treacheries.itLikesRiddles, Treacheries.whatsInMyPocket]

-- | The first player names the next item, or, all named, shuffles his deck and discards the
-- top cards; the players may then act before the matches are counted.
nameNext :: ReverseQueue m => RiddleState -> m ()
nameNext r = case r.riddleToName of
  RiddleCardType : _ -> chooseOneM lead do
    questionLabeled "riddle.nameCardType"
    causedBy card
    for_ [("ally", NamedCardType AllyType), ("event", NamedCardType EventType), ("attachment", NamedCardType AttachmentType), ("treasure", NamedTreasure)] \(k, n) ->
      labeled ("riddle." <> k) $ name n
  RiddleSphere : _ -> chooseOneM lead do
    questionLabeled "riddle.nameSphere"
    causedBy card
    for_ [("leadership", Leadership), ("spirit", Spirit), ("lore", Lore), ("tactics", Tactics), ("baggins", Baggins)] \(k, sphere) ->
      labeled ("riddle." <> k) $ name (NamedSphere sphere)
  -- any cost may be named; no player card is printed with more than 9
  RiddleCost : _ -> chooseAmountM lead "riddle.nameCost" card 0 9 (name . NamedCost)
  [] -> do
    shufflePlayerDeck lead
    riddleDiscard r.riddleDiscards
    pushAll . map OptionalActionWindow =<< getPlayerOrder
    push $ ScenarioSpecific "riddle.resolve" Null
 where
  lead = r.riddleAnswerer
  card = CardIdTarget r.riddleCard.id
  name :: ReverseQueue m => Named -> m ()
  name = push . ScenarioSpecific "riddle.name" . toJSON
