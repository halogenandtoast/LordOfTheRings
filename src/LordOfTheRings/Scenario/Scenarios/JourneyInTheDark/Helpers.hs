{- | What Journey in the Dark's cards share: Doom, Doom, Doom's damage, which they remove,
The Balrog's attacks, and the burdens the campaign card hands out.
-}
module LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (
  getDoom,
  getBalrog,
  removeDoomDamage,
  balrogAttacks,
  burdenTreacheries,
  burdenObjectives,
  earnBurdens,
) where

import LordOfTheRings.Campaign (CampaignLog, CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

-- | Doom, Doom, Doom, while it is in play.
getDoom :: HasGame m => m (Maybe ObjectiveId)
getDoom = getsGame (fmap toId . find ((== toCardCode Objectives.doomDoomDoom) . toCardCode) . toList . (.entities.objectives))

-- | The Balrog, while it is in play.
getBalrog :: HasGame m => m (Maybe EnemyId)
getBalrog = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.theBalrog)

-- | Remove 1 damage token from Doom, Doom, Doom, if it has one.
removeDoomDamage :: (ReverseQueue m, Sourceable source) => source -> m ()
removeDoomDamage source =
  getDoom >>= traverse_ \oid -> do
    damage <- getDamage (ObjectiveTarget oid)
    when (damage > 0) $ removeTokens source (ObjectiveTarget oid) Damage 1

{- | The Balrog makes an attack, if it is in play: against the first player, with whom it is
engaged from the staging area.
-}
balrogAttacks :: ReverseQueue m => m ()
balrogAttacks =
  getBalrog >>= traverse_ \balrog -> do
    e <- getEnemy balrog
    lead <- getLead
    immediateAttack (fromMaybe lead $ placementController e.placement) balrog

burdenTreacheries :: [CardDef]
burdenTreacheries = [Treacheries.pursuedByTheEnemy, Treacheries.shadowOfFearJourneyInTheDark]

burdenObjectives :: [CardDef]
burdenObjectives = [Objectives.grievousWound, Objectives.overcomeByGrief]

{- | Resolution: The first player must choose one of the burden treachery cards (Pursued by
the Enemy or Shadow of Fear), and one of the burden objective cards (Grievous Wound or
Overcome by Grief) to add to the Campaign Pool. If The Balrog is still in play, the first
player must choose all 4 burden cards instead. The chosen burdens have been earned by the
players.

The scenario asks as the last stage is defeated, before the game is won, as the game over
asks nothing. A burden already in the pool is not added again.
-}
earnBurdens :: ReverseQueue m => m ()
earnBurdens = getsGame (scenarioCampaign . toAttrs . (.scenario)) >>= traverse_ \l -> do
  balrog <- isJust <$> getBalrog
  if balrog
    then traverse_ (earn l) (burdenTreacheries <> burdenObjectives)
    else do
      lead <- getLead
      for_ [("journeyInTheDark.earnTreachery", burdenTreacheries), ("journeyInTheDark.earnObjective", burdenObjectives)] \(question, defs) ->
        chooseOneM lead do
          questionLabeled question
          for_ defs \def -> labeled ("journeyInTheDark." <> burdenKey def) $ earn l def
 where
  earn :: ReverseQueue n => CampaignLog -> CardDef -> n ()
  earn l def = unless (toCardCode def `elem` map (.code) l.pool) $ updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) Nothing Nothing
  burdenKey def
    | def == Treacheries.pursuedByTheEnemy = "pursuedByTheEnemy"
    | def == Treacheries.shadowOfFearJourneyInTheDark = "shadowOfFear"
    | def == Objectives.grievousWound = "grievousWound"
    | otherwise = "overcomeByGrief"
