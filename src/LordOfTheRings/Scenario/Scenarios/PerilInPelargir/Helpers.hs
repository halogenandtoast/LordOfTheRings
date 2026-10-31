-- | What the cards of Peril in Pelargir share: Alcaron's Scroll, and how a Brigand engaged.
module LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (
  getScroll,
  scrollHero,
  scrollHolder,
  scrollOnHero,
  damageToScrollHero,
  wasOptionallyEngaged,
  scrollEnemy,
  enemyProgress,
) where

import LordOfTheRings.Card (toCardCode)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (HasQueue, peekQueue)
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Id (EnemyId, ObjectiveId, PlayerId)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Token (Token (Progress), countTokens)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (DamageRedirectedTo, UndefendedDamageTo))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Scroll))
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (OptionallyEngaged))

-- | Alcaron's Scroll, while it is in play.
getScroll :: HasGame m => m (Maybe ObjectiveId)
getScroll =
  getsGame
    $ fmap toId
    . find ((== toCardCode Objectives.alcaronsScroll) . toCardCode)
    . toList
    . (.entities.objectives)

-- | The hero Alcaron's Scroll is attached to.
scrollHero :: HasGame m => m (Maybe Target)
scrollHero = fmap toTarget . headMay <$> select (HeroWithAttached Scroll)

-- | The player who controls Alcaron's Scroll: the one whose hero has it attached.
scrollHolder :: HasGame m => m (Maybe PlayerId)
scrollHolder = maybe (pure Nothing) characterController =<< scrollHero

scrollOnHero :: HasGame m => m Bool
scrollOnHero = isJust <$> scrollHero

{- | Shadow: Any damage dealt by this attack is dealt to the hero with Alcaron's Scroll
attached, if able. The attack's defenders, none when undefended.
-}
damageToScrollHero :: ReverseQueue m => EnemyId -> [Target] -> m ()
damageToScrollHero eid defenders =
  scrollHero >>= traverse_ \hero ->
    if null defenders
      then attackModifier (EnemySource eid) eid (UndefendedDamageTo hero)
      else for_ (filter (/= hero) defenders) \d -> attackModifier (EnemySource eid) d (DamageRedirectedTo hero)

{- | Whether the player engaged the enemy in the optional engagement step: that window is
still waiting on the game's queue while the enemy's own engagement resolves.
-}
wasOptionallyEngaged :: (MonadTrans t, HasQueue Message m) => EnemyId -> PlayerId -> t m Bool
wasOptionallyEngaged eid pid = lift $ any optionally <$> peekQueue
 where
  optionally = \case
    CheckWindows ws -> Window After (OptionallyEngaged eid pid) `elem` ws
    _ -> False

-- | The enemy Alcaron's Scroll is attached to (Nightmare: it escapes with it).
scrollEnemy :: HasGame m => m (Maybe EnemyId)
scrollEnemy =
  getsGame \g ->
    headMay
      [ eid
      | o <- toList g.entities.objectives
      , toCardCode o == toCardCode Objectives.alcaronsScroll
      , AttachedTo (EnemyTarget eid) <- [o.placement]
      ]

-- | The progress tokens on the enemy, as the Nightmare setup card counts them.
enemyProgress :: HasGame m => EnemyId -> m Int
enemyProgress eid = getsGame (maybe 0 (countTokens Progress . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))
