{- | What the cards of To Catch an Orc share: each player's out-of-play deck, Searches X,
and Mugash, in play as an enemy or attached to a hero as a Captive.
-}
module LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (
  getOutOfPlayDecks,
  outOfPlayDeckOf,
  setUpOutOfPlayDecks,
  searches,
  revealTopOfOutOfPlayDeck,
  discardTopOfOutOfPlayDeck,
  shuffleRandomCardIntoOutOfPlayDeck,
  getMugash,
  mugashHost,
  mugashCaptor,
  engageCaptor,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (Enemy)
import LordOfTheRings.Entities (entitiesEnemies)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, EngagedWith))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Target

{- | Each player's out-of-play deck, kept as the scenario's meta: the top 20 cards of his
deck set aside at setup, with Mugash or one of his Guards shuffled in.
-}
getOutOfPlayDecks :: ScenarioAttrs -> Map PlayerId [Card]
getOutOfPlayDecks attrs = fromMaybe mempty $ parseMaybe parseJSON attrs.scenarioMeta

outOfPlayDeckOf :: HasGame m => PlayerId -> m [Card]
outOfPlayDeckOf pid = getsGame (Map.findWithDefault [] pid . getOutOfPlayDecks . toAttrs . gameScenario)

-- | Orders from Orthanc's setup: the scenario sets aside each player's out-of-play deck.
setUpOutOfPlayDecks :: ReverseQueue m => m ()
setUpOutOfPlayDecks = push $ ScenarioSpecific "outOfPlay.setup" Null

{- | The player Searches X: he reveals the top X cards of his out-of-play deck, adds each
revealed enemy to the staging area, chooses 1 player card to take into his hand, and
discards the rest.
-}
searches :: ReverseQueue m => PlayerId -> Int -> m ()
searches pid n = push $ ScenarioSpecific "outOfPlay.search" (toJSON (pid, n))

-- | Mugash's Lair: the top card of the player's out-of-play deck is revealed, an enemy
-- added to the staging area and anything else discarded.
revealTopOfOutOfPlayDeck :: ReverseQueue m => PlayerId -> m ()
revealTopOfOutOfPlayDeck pid = push $ ScenarioSpecific "outOfPlay.reveal" (toJSON pid)

-- | Mugash's Cunning: the player discards the top X cards of his out-of-play deck, each
-- enemy among them added to the staging area.
discardTopOfOutOfPlayDeck :: ReverseQueue m => PlayerId -> Int -> m ()
discardTopOfOutOfPlayDeck pid n = push $ ScenarioSpecific "outOfPlay.discardTop" (toJSON (pid, n))

-- | Methedras Orc: the player shuffles 1 random card from his hand into his out-of-play deck.
shuffleRandomCardIntoOutOfPlayDeck :: ReverseQueue m => PlayerId -> m ()
shuffleRandomCardIntoOutOfPlayDeck pid = push $ ScenarioSpecific "outOfPlay.shuffleFromHand" (toJSON pid)

-- | Mugash, in play: as an enemy, or attached to a hero.
getMugash :: HasGame m => m (Maybe Enemy)
getMugash =
  getsGame (find ((== toCardCode Enemies.mugashEnemy) . toCardCode) . toList . entitiesEnemies . gameEntities)

-- | The hero Mugash is attached to, as a Captive.
mugashHost :: HasGame m => m (Maybe Target)
mugashHost =
  getMugash <&> \m -> case (.placement) <$> m of
    Just (AttachedTo host@(HeroTarget _)) -> Just host
    _ -> Nothing

-- | The player who controls the hero Mugash is attached to.
mugashCaptor :: HasGame m => m (Maybe PlayerId)
mugashCaptor = maybe (pure Nothing) characterController =<< mugashHost

-- | Mugash's Guard engages the player who controls the hero Mugash is attached to.
engageCaptor :: ReverseQueue m => EnemyId -> m ()
engageCaptor eid = do
  captor <- mugashCaptor
  placement <- (.placement) <$> getEnemy eid
  for_ captor \p -> when (placement /= EngagedWith p) $ engageEnemy p eid
