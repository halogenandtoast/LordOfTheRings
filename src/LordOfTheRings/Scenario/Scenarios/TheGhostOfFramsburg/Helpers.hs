-- | What the cards of The Ghost of Framsburg share: its Loot objectives, and the first
-- player's threat.
module LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (
  isLoot,
  lootObjectives,
  lootControlled,
  firstPlayersTensDigit,
  lootStage,
  lootObjective,
) where

import LordOfTheRings.Card (HasCardDef, cdKeywords, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modifySelf)
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Keyword (Keyword (Loot))
import LordOfTheRings.Matcher (getLead, getThreat)
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated, FirstPlayerClaimsWhenFree))
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Target (Target (HeroTarget))

isLoot :: HasCardDef a => a -> Bool
isLoot = (Loot `member`) . cdKeywords . toCardDef

-- | The Loot objectives in play.
lootObjectives :: HasGame m => m [ObjectiveAttrs]
lootObjectives = getsGame (filter isLoot . map toAttrs . toList . entitiesObjectives . gameEntities)

-- | How many Loot objectives the players control: those attached to their heroes.
lootControlled :: HasGame m => m Int
lootControlled = length . filter (\o -> case o.placement of AttachedTo (HeroTarget _) -> True; _ -> False) <$> lootObjectives

-- | "X is the tens digit of the first player's threat."
firstPlayersTensDigit :: HasGame m => m Int
firstPlayersTensDigit = (`div` 10) <$> (getThreat =<< getLead)

-- | "This stage cannot be defeated unless the players control at least N Loot objectives."
lootStage :: HasModifiersM m => QuestAttrs -> Int -> m ()
lootStage a n = do
  controlled <- lootControlled
  when (controlled < n) $ modifySelf a [CannotBeDefeated]

-- | "If unattached, the first player claims it and attaches it to a hero he controls": once
-- nothing guards it.
lootObjective :: HasModifiersM m => ObjectiveAttrs -> m ()
lootObjective a = case a.placement of
  AttachedTo _ -> pure ()
  _ -> modifySelf a [FirstPlayerClaimsWhenFree]
