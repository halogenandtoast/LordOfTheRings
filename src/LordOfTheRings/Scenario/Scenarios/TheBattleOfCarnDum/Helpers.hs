-- | What the cards of The Battle of Carn Dûm share: Thaurdir, whose two sides are one card
-- turned over, and the shadow cards they count.
module LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (
  thaurdirCaptain,
  thaurdirChampion,
  getThaurdir,
  isThaurdir,
  flipThaurdir,
  shadowCardsInPlay,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (Flip))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Thaurdir, Captain side faceup.
thaurdirCaptain :: EnemyMatcher
thaurdirCaptain = EnemyWithCardCode (toCardCode Enemies.thaurdirCaptain)

-- | Thaurdir, Champion side faceup.
thaurdirChampion :: EnemyMatcher
thaurdirChampion = EnemyWithCardCode (toCardCode Enemies.thaurdirChampion)

-- | Thaurdir, whichever side is faceup.
getThaurdir :: HasGame m => m (Maybe EnemyId)
getThaurdir = headMay <$> ((<>) <$> select thaurdirCaptain <*> select thaurdirChampion)

isThaurdir :: HasGame m => EnemyId -> m Bool
isThaurdir eid = (== Just eid) <$> getThaurdir

-- | Turn Thaurdir over to his other side.
flipThaurdir :: ReverseQueue m => m ()
flipThaurdir = getThaurdir >>= traverse_ (push . Flip . toTarget)

-- | The shadow cards dealt to the enemies in play.
shadowCardsInPlay :: HasGame m => m Int
shadowCardsInPlay = getsGame (sum . map (length . (.shadowCards)) . toList . (.entities.enemies))
