-- | What the cards of The Dungeons of Cirith Gurat share: captives taken by encounter cards.
module LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (
  unattachedInStaging,
  guardingAnObjective,
  discardUntilAlly,
  takeCaptive,
  captiveOfStaged,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card, CardType (AllyType), IsCard (toCard), cdCardType, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Hero.Types (HeroAttrs (heroGuardedBy))
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

{- | The enemies, and the locations if asked for, in the staging area with nothing attached:
no attachment, no Mount and no captive, as "an unattached enemy in the staging area" reads.
-}
unattachedInStaging :: HasGame m => Bool -> m [Target]
unattachedInStaging withLocations = do
  es <- getsGame gameEntities
  enemies <- map toTarget <$> select EnemyInStagingArea
  locations <- if withLocations then map toTarget <$> select LocationInStagingArea else pure []
  let cardOf t = case t of
        EnemyTarget eid -> (.id) . toCard <$> Map.lookup eid (entitiesEnemies es)
        LocationTarget lid -> (.id) . toCard <$> Map.lookup lid (entitiesLocations es)
        _ -> Nothing
      held t =
        any ((== AttachedTo t) . (.placement)) (toList $ entitiesEnemies es)
          || maybe False (\cid -> any ((== Just cid) . heroGuardedBy . toAttrs) (toList $ entitiesHeroes es) || any ((== AttachedTo (CardIdTarget cid)) . (.placement)) (toList $ entitiesAllies es)) (cardOf t)
  flip filterM (enemies <> locations) \t -> do
    attachments <- getAttachmentsOn t
    pure $ null attachments && not (held t)

-- | Whether the encounter card guards a captive, as "while Prison Guard is guarding an
-- objective" reads: the only guarded objectives of this scenario are its captives.
guardingAnObjective :: (HasGame m, IsCard a) => a -> m Bool
guardingAnObjective = fmap notNull . charactersGuardedBy . (.id) . toCard

{- | The player discards cards from the top of his deck until he discards an ally that
matches, as Torture Room reads, and that ally: with none in his deck he discards it all.
-}
discardUntilAlly :: ReverseQueue m => PlayerId -> (Card -> m Bool) -> m (Maybe Card)
discardUntilAlly pid ok = do
  deck <- (.deck) <$> getPlayer pid
  found <- findM (\c -> (cdCardType (toCardDef c) == AllyType &&) <$> ok c) deck
  let n = maybe (length deck) (\c -> maybe (length deck) (+ 1) (findIndex ((== c.id) . (.id)) deck)) found
  when (n > 0) $ discardTopOfDeck pid n
  pure found
 where
  findM p = foldr (\x rest -> p x >>= \b -> if b then pure (Just x) else rest) (pure Nothing)

{- | The player attaches the ally card, out of play, to one of the encounter cards as its
captive, as "attaches that ally to an unattached enemy in the staging area" reads.
-}
takeCaptive :: (ReverseQueue m, Targetable source) => source -> PlayerId -> Card -> [Target] -> m ()
takeCaptive source pid card captors = chooseOrRunOneM pid do
  questionLabeled "theDungeonsOfCirithGurat.attachCaptive"
  causedBy source
  targets captors \t -> do
    captor <- captorCard t
    for_ captor $ guardCardWith card

-- | The character in play becomes the captive of an encounter card in the staging area.
captiveOfStaged :: (ReverseQueue m, Targetable character) => character -> Target -> m ()
captiveOfStaged c t = do
  captor <- captorCard t
  for_ captor $ guardCharacterWith c

captorCard :: HasGame m => Target -> m (Maybe Card)
captorCard = \case
  EnemyTarget eid -> Just . toCard <$> getEnemy eid
  LocationTarget lid -> Just . toCard <$> getLocation lid
  _ -> pure Nothing
