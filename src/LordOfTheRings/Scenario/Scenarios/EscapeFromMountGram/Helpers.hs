{- | What the cards of Escape from Mount Gram share: the captured decks and the cards
captured underneath encounter and quest cards.

Each player has a captured deck, prepared at setup from his allies and his Item, Mount
and Artifact attachments, with the heroes not chosen to start: a random one of those on
top. A card with Capture X, as it enters play, captures the top X cards of each of its
players' captured decks facedown underneath it (only those at its stage, while the
players are apart). A card can also be captured from play, as Cruel Torturer captures a
character: it leaves play, its tokens and attachments discarded. Captured cards are out
of play, under no one's control. When the card holding them leaves play they are rescued:
each goes to its owner's hand, a hero into play under its owner's control.

The scenario keeps the captured decks and what is under each card, by the card's id, in
its meta; the board is sent only how many.
-}
module LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (
  Meta (..),
  getMeta,
  capturedDeck,
  capturedUnder,
  cardsWithCaptured,
  capturedTarget,
  captureFromDeck,
  rescueAtRandom,
  putIntoPlayAtNoCost,
  releaseCaptured,
  discardCaptured,
  discardTopOfCapturedDeck,
  shuffleFromHandIntoCapturedDeck,
  prepareCapturedDeck,
  chooseStartingHero,
  isCapturable,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (Entities (..))
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Queue (ReverseQueue)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Artifact, Item, Mount))

data Meta = Meta
  { capturedDecks :: Map PlayerId [PlayerCard]
  , captured :: Map CardId [PlayerCard]
  -- ^ the cards captured underneath each card in play, by its card id
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta mempty mempty) $ parseMaybe parseJSON attrs.meta

gameMeta :: HasGame m => m Meta
gameMeta = getsGame (getMeta . toAttrs . gameScenario)

-- | The player's captured deck, its top first.
capturedDeck :: HasGame m => PlayerId -> m [PlayerCard]
capturedDeck pid = Map.findWithDefault [] pid . (.capturedDecks) <$> gameMeta

-- | The cards captured underneath the card.
capturedUnder :: HasGame m => CardId -> m [PlayerCard]
capturedUnder cid = Map.findWithDefault [] cid . (.captured) <$> gameMeta

-- | How many cards in play have 1 or more captured cards underneath them, as Goblin
-- Tormentor's X reads: those in the area in focus.
cardsWithCaptured :: HasGame m => m Int
cardsWithCaptured = do
  meta <- gameMeta
  es <- getsGame gameEntities
  let ids =
        map (.id) $ map toCard (toList es.entitiesEnemies) <> map toCard (toList es.entitiesLocations) <> map toCard (toList es.entitiesQuests)
  pure $ length [cid | cid <- ids, notNull (Map.findWithDefault [] cid meta.captured)]

-- | The card in play, in the area in focus, that can hold captured cards, as its target and
-- its card; a card named by its card id is found wherever it is.
capturedTarget :: HasGame m => Target -> m (Maybe (Target, Card))
capturedTarget t = getsGame \g ->
  let es = gameEntities g
      find' :: (IsCard v, Targetable v) => Map k v -> Maybe (Target, Card)
      find' m = case t of
        CardIdTarget cid -> (\v -> (toTarget v, toCard v)) <$> find ((== cid) . (.id) . toCard) (toList m)
        _ -> (\v -> (toTarget v, toCard v)) <$> find ((== t) . toTarget) (toList m)
   in find' es.entitiesEnemies <|> find' es.entitiesLocations <|> find' es.entitiesQuests

-- | Each player at the card's stage captures the top cards of his captured deck
-- underneath it.
captureFromDeck :: (ReverseQueue m, Targetable host) => host -> Int -> m ()
captureFromDeck host n = when (n > 0) $ push $ ScenarioSpecific "mountGram.capture" (toJSON (toTarget host, n))

-- | Rescue this many of the cards captured underneath the card, at random.
rescueAtRandom :: ReverseQueue m => CardId -> Int -> m ()
rescueAtRandom cid n = when (n > 0) $ push $ ScenarioSpecific "mountGram.rescueAtRandom" (toJSON (cid, n))

-- | The card taken from underneath the card is put into play at no cost, as Prison Cell's.
releaseCaptured :: ReverseQueue m => CardId -> CardId -> m ()
releaseCaptured host cid = push $ ScenarioSpecific "mountGram.release" (toJSON (host, cid))

-- | The cards captured underneath the card are discarded, as Stop the Executioners!'s.
discardCaptured :: ReverseQueue m => CardId -> m ()
discardCaptured host = push $ ScenarioSpecific "mountGram.discardCaptured" (toJSON host)

-- | The top card of the player's captured deck goes to his discard pile.
discardTopOfCapturedDeck :: ReverseQueue m => PlayerId -> m ()
discardTopOfCapturedDeck pid = push $ ScenarioSpecific "mountGram.discardTop" (toJSON pid)

-- | Cards in the player's hand are shuffled into his captured deck.
shuffleFromHandIntoCapturedDeck :: ReverseQueue m => PlayerId -> [PlayerCard] -> m ()
shuffleFromHandIntoCapturedDeck pid cards = unless (null cards) do
  for_ cards \pc -> push $ RemoveFromHand pid pc.id
  push $ ScenarioSpecific "mountGram.shuffleIn" (toJSON (pid, cards))

-- | The player shuffles his hand into his deck and prepares his captured deck from it.
prepareCapturedDeck :: ReverseQueue m => PlayerId -> m ()
prepareCapturedDeck pid = push $ ScenarioSpecific "mountGram.prepare" (toJSON pid)

-- | The hero is the player's starting hero: his others go to his captured deck.
chooseStartingHero :: ReverseQueue m => PlayerId -> HeroId -> m ()
chooseStartingHero pid hid = push $ ScenarioSpecific "mountGram.startingHero" (toJSON (pid, hid))

-- | An ally, or an Item, Mount or Artifact attachment, goes to the captured deck.
isCapturable :: HasCardDef a => a -> Bool
isCapturable c = case cdCardType def of
  AllyType -> True
  AttachmentType -> any (`member` cdCardTraits def) [Item, Mount, Artifact]
  _ -> False
 where
  def = toCardDef c

{- | A card put into play at no cost under its owner's control: a hero or an ally, or an
attachment, which its owner attaches.
-}
putIntoPlayAtNoCost :: ReverseQueue m => PlayerCard -> m ()
putIntoPlayAtNoCost pc = for_ pc.owner \owner -> push case cdCardType (toCardDef pc) of
  HeroType -> PutHeroIntoPlay owner pc
  AttachmentType -> PutAttachmentIntoPlay owner pc
  _ -> PutAllyIntoPlayUnder owner pc
