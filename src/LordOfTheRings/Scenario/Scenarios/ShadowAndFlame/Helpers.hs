-- | What the cards of Shadow and Flame share: Durin's Bane, and the first player's choice to
-- cancel its effects by removing a questing hero.
module LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (
  durinsBane,
  getDurinsBane,
  isDurinsBane,
  durinsBaneHitPointsLeft,
  unlessQuestingHeroLeaves,
  attachToDurinsBane,
) where

import LordOfTheRings.Card (cdUnique, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Target
import LordOfTheRings.Treachery.Types (TreacheryAttrs (..))

durinsBane :: EnemyMatcher
durinsBane = EnemyWithCardCode (toCardCode Enemies.durinsBane)

getDurinsBane :: HasGame m => m (Maybe EnemyId)
getDurinsBane = selectOne durinsBane

isDurinsBane :: HasGame m => EnemyId -> m Bool
isDurinsBane eid = selectAny (EnemyWithId eid <> durinsBane)

-- | Its remaining hit points: 0 or less once its damage reaches them, as it is Indestructible.
durinsBaneHitPointsLeft :: HasGame m => EnemyId -> m Int
durinsBaneHitPointsLeft eid = (-) <$> getEnemyHitPoints eid <*> getDamage (toTarget eid)

{- | The effect resolves unless the first player removes 1 questing hero he controls from
the quest to cancel it, as Inner Flame and Inner Shadow read.
-}
unlessQuestingHeroLeaves
  :: (ReverseQueue m, Targetable source) => source -> QueueT Message m () -> m ()
unlessQuestingHeroLeaves source effect = do
  lead <- getLead
  questing <- select (HeroControlledBy (PlayerWithId lead) <> CommittedHero)
  chooseOrRunOneM lead do
    questionLabeled "shadowAndFlame.removeQuestingHero"
    causedBy source
    labeled "shadowAndFlame.resolveEffect" effect
    targets questing removeFromQuest

{- | The treachery attaches itself to Durin's Bane, as Fiery Sword and Many Thonged Whip
read. A unique one with a copy already on it does not attach, and is discarded; Counter-spell
is not unique, so each copy attaches.
-}
attachToDurinsBane :: ReverseQueue m => TreacheryAttrs -> m ()
attachToDurinsBane attrs = do
  bane <- getDurinsBane
  for_ bane \eid -> do
    copies <- treacheriesOn attrs.treacheryCardCode (toTarget eid)
    when (null copies || not (cdUnique $ toCardDef attrs)) $ attachTreachery attrs.treacheryId eid
