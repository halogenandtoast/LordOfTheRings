-- | What the cards of The Dread Realm share: the Sorcery cards in play, and reanimating
-- player cards as Reanimated Dead.
module LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (
  sorceryCardsInPlay,
  reanimatedDeadInPlay,
  reanimateWithShadow,
  reanimateTopOfDeck,
  reanimateDestroyedBy,
) where

import LordOfTheRings.Card (Card (PlayerCard), toCardCode, toCardDef)
import LordOfTheRings.Card.Synthetic qualified as Synthetic
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesEnemies, entitiesTreacheries)
import LordOfTheRings.Game.Base (AttackDestroyed (..), gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, Unplaced))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Sorcery))

{- | The Sorcery cards in play: encounter cards attached as conditions, or parked between
stages, and a Wraith of Carn Dûm attached as a Sorcery attachment. A treachery still
resolving its revelation is not yet in play.
-}
sorceryCardsInPlay :: HasGame m => m [Target]
sorceryCardsInPlay = do
  es <- getsGame gameEntities
  treacheries <- filterM (\t -> hasTrait Sorcery (toTarget t) (toCardDef t)) [t | t <- toList (entitiesTreacheries es), t.placement /= Unplaced]
  -- the Wraith read by its card, not the trait it gains, as the modifiers that give the
  -- trait are worked out alongside Daechanar's, which count it
  let enemies = [e | e <- toList (entitiesEnemies es), isAttached e.placement, toCardCode e == toCardCode Enemies.wraithOfCarnDum || Sorcery `member` (toCardDef e).traits]
  pure $ map toTarget treacheries <> map toTarget enemies
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

reanimatedDeadInPlay :: HasGame m => m [EnemyId]
reanimatedDeadInPlay = select (EnemyWithCardCode $ toCardCode Synthetic.reanimatedDead)

-- | The card is reanimated, and the Reanimated Dead is dealt 1 shadow card.
reanimateWithShadow :: ReverseQueue m => CardId -> m ()
reanimateWithShadow cid = do
  eid <- reanimate cid
  whileInPlay (EnemyTarget eid) $ dealShadowCard eid

-- | The player reanimates the top N cards of their deck.
reanimateTopOfDeck :: ReverseQueue m => PlayerId -> Int -> m ()
reanimateTopOfDeck pid n = do
  deck <- (.deck) <$> getPlayer pid
  traverse_ (reanimate . (.id)) (take n deck)

{- | Each character the enemy's attack destroyed, or only those given, is reanimated and
dealt 1 shadow card, as "if this attack destroys a character, reanimate that character"
reads.
-}
reanimateDestroyedBy :: ReverseQueue m => EnemyId -> Maybe [Target] -> m ()
reanimateDestroyedBy eid only = do
  destroyed <- attackDestroyed eid
  for_ destroyed \d -> case d.attackDestroyedCard of
    PlayerCard c | maybe True (d.attackDestroyedTarget `elem`) only -> reanimateWithShadow c.id
    _ -> pure ()
