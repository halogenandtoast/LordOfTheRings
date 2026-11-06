module LordOfTheRings.Helpers.HandEntry (handEntryAttempt, handEntrySucceeded) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Game.Base
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Player.Runner (topOfDeckPlayers)
import LordOfTheRings.Prelude

-- Only the initiating message is counted: its later entry message sees the card
-- already removed from hand. Discards and transfers between hands do not count.
handEntryAttempt :: Game -> Message -> GameT (Maybe (PlayerId, CardId))
handEntryAttempt g = \case
  PlayCardInWindow pid cid _ -> case Map.lookup pid g.players of
    Just player -> do
      -- Caleb Grace (4 May 2019): Gandalf plays count as played from hand.
      -- https://ffg-forum-archive.entropicdreams.com/topic/277776-unofficial-official-rulings-thread/page/2/
      fromTop <- notNull <$> topOfDeckPlayers player
      pure $ guard (inHand pid cid || (fromTop && Just cid == fmap (.id) (headMay player.deck))) $> (pid, cid)
    Nothing -> pure Nothing
  PlayFromHandAtNoCost pid cid -> held pid cid
  PlayFromHandOnAtNoCost pid cid _ -> held pid cid
  PutAllyIntoPlay pid cid -> held pid cid
  PutAttachmentIntoPlayFromHand pid cid _ -> held pid cid
  PutAllyIntoPlayUnder pid card -> held pid card.id
  PutAttachmentIntoPlay pid card -> held pid card.id
  PlayAttachmentOn pid card _ -> held pid card.id
  PlayAtNoCost pid card -> held pid card.id
  PutHeroIntoPlay pid card -> held pid card.id
  PutHeroIntoPlayAs _ pid card -> held pid card.id
  PutSideQuestIntoPlay pid card -> held pid card.id
  AttachEncounterCardFromHand pid card _ -> held pid card.id
  PutObjectiveAllyIntoPlayUnder pid card -> held pid card.id
  _ -> pure Nothing
 where
  inHand pid cid = maybe False (any ((== cid) . (.id)) . (.hand)) $ Map.lookup pid g.players
  held pid cid = pure $ guard (inHand pid cid) $> (pid, cid)

-- A prevented put stays in hand and spends no allowance. A successful play may
-- go straight to discard (an event), whereas a put creates an entity in play.
handEntrySucceeded :: Game -> Game -> (PlayerId, CardId) -> Bool
handEntrySucceeded old new (pid, cid) = consumed || entered
 where
  held g = maybe [] (map (.id) . (\p -> p.hand <> p.deck)) $ Map.lookup pid g.players
  consumed = cid `elem` held old && cid `notElem` held new
  entered = cid `elem` entityIds new && cid `notElem` entityIds old
  entityIds g = concat
    [ map ((.id) . toCard) $ toList g.entities.heroes
    , map ((.id) . toCard) $ toList g.entities.allies
    , map ((.id) . toCard) $ toList g.entities.attachments
    , map ((.id) . toCard) $ toList g.entities.quests
    , map ((.id) . toCard) $ toList g.entities.objectives
    , map ((.id) . toCard) $ toList g.entities.treacheries
    , map ((.id) . toCard) $ toList g.entities.enemies
    ]
