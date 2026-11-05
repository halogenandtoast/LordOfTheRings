module LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim (dungeonsDeepAndCavernsDim) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, getLead, getPlayer, getPlayerOrder, select)
import LordOfTheRings.Message.Lifted.Choose
import LordOfTheRings.Modifier (ModifierType (AnsweringRiddle, CannotTakeDamage))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers
import LordOfTheRings.Source (Source (CardIdSource, ScenarioSource))
import LordOfTheRings.Token (Token (Progress))
import LordOfTheRings.Trait (Trait, traitName)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype DungeonsDeepAndCavernsDim = DungeonsDeepAndCavernsDim ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonsDeepAndCavernsDim :: Scenario
dungeonsDeepAndCavernsDim =
  Scenario
    $ scenario
      (DungeonsDeepAndCavernsDim . (nightmareDeckL ?~ nightmare))
      "dungeons-deep-and-caverns-dim"
      "Dungeons Deep and Caverns Dim"
 where
  -- https://hallofbeorn.com/LotR/Details/Dungeons-Deep-and-Caverns-Dim-Nightmare-DDaCDN
  nightmare =
    nightmareDeck
      Nightmare.dungeonsDeepAndCavernsDimNightmare
      EncounterSet.DungeonsDeepAndCavernsDimNightmare
      [ (Enemies.goblinMiners, 3)
      , (Enemies.goblinAxeman, 3)
      , (Locations.theGoblinsCaves, 3)
      , (Treacheries.gripGrabPinchNab, 3)
      , (Treacheries.notFairNotFair, 1)
      ]

{- | Once the items of a riddle are named and its cards discarded, the players may take
actions, as Bilbo's Magic Ring's first, before its matches are counted.
-}
instance HasModifiersFor DungeonsDeepAndCavernsDim where
  getModifiersFor (DungeonsDeepAndCavernsDim a) = do
    when a.nightmare do
      gollums <- select gollumEnemy
      for_ gollums \g -> modified a g [CannotTakeDamage]
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
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.DungeonsDeepAndCavernsDim, EncounterSet.MistyMountainGoblins]
      firstStage <- genEncounterCard Quests.outOfTheFryingPan
      laterStages <- traverse genEncounterCard [Quests.riddlesInTheDark, Quests.intoTheFire]
      putQuestIntoPlay firstStage
      let (gate, deck) = partition (\c -> attrs.nightmare && toCardCode c == toCardCode Locations.theLowerGate) cards
      pure
        $ DungeonsDeepAndCavernsDim
        $ attrs
        & encounterDeckL
        .~ deck
        & setAsideL
        .~ gate
        & questDeckL
        .~ laterStages
    Flip target | attrs.nightmare -> do
      turning <-
        getsGame
          $ any
            ( \q ->
                toCardCode q
                  == toCardCode Quests.intoTheFire
                  && not q.flipped
                  && (isTarget q target || CardIdTarget (toCard q).id == target)
            )
          . toList
          . (.entities.quests)
      when turning $ addSetAsideToStaging (cardIs Locations.theLowerGate)
      DungeonsDeepAndCavernsDim <$> runMessage msg attrs
    RevealEncounterCard
      | card : rest <- attrs.encounterDeck
      , hasRiddle card
      , not (answeredAsItResolves card) -> do
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
    ScenarioSpecific
      "riddle.answer"
      (parseMaybe parseJSON -> Just (source :: Source, fromReveal, card, Riddle items n)) -> do
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

{- | The first player names the next item, or, all named, shuffles his deck and discards the
top cards; the players may then act before the matches are counted.
-}
nameNext :: ReverseQueue m => RiddleState -> m ()
nameNext r = case r.riddleToName of
  RiddleCardType : _ -> chooseOneM lead do
    questionLabeled "riddle.nameCardType"
    causedBy card
    for_
      [ ("ally", NamedCardType AllyType)
      , ("event", NamedCardType EventType)
      , ("attachment", NamedCardType AttachmentType)
      , ("treasure", NamedTreasure)
      ]
      \(k, n) ->
        labeled ("riddle." <> k) $ name n
  RiddleSphere : _ -> chooseOneM lead do
    questionLabeled "riddle.nameSphere"
    causedBy card
    for_
      [ ("leadership", Leadership)
      , ("spirit", Spirit)
      , ("lore", Lore)
      , ("tactics", Tactics)
      , ("baggins", Baggins)
      ]
      \(k, sphere) ->
        labeled ("riddle." <> k) $ name (NamedSphere sphere)
  -- any cost may be named; no player card is printed with more than 9
  RiddleCost : _ -> chooseAmountM lead "riddle.nameCost" card 0 9 (name . NamedCost)
  RiddleTitle : _ -> chooseOneM lead do
    questionLabeled "riddle.nameTitle"
    causedBy card
    let titles =
          sort
            $ ordNub
              [title | def <- toList allPlayerCards <> toList allEncounterCards, let Name title _ = cdName def]
    for_ titles \title -> labeledWith "riddle.namedTitle" [textVar "title" title] $ name (NamedTitle title)
  RiddleTrait : _ -> chooseOneM lead do
    questionLabeled "riddle.nameTrait"
    causedBy card
    for_ ([minBound .. maxBound] :: [Trait]) \trait ->
      labeledWith "riddle.namedTrait" [textVar "trait" (traitName trait)] $ name (NamedTrait trait)
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
