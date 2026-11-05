module LordOfTheRings.Helpers.Engagement (highestToughnessEngagements) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Toughness))
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Find the highest Toughness first, then legal engagements among enemies tied
-- at that value. An unengageable toughest enemy does not permit a weaker choice.
highestToughnessEngagements :: HasGame m => m [(PlayerId, EnemyId)]
highestToughnessEngagements = do
  players <- filterM (fmap (not . (.eliminated)) . getPlayer) =<< getStagePlayers
  enemies <- select EnemyInStagingArea
  values <- for enemies \eid -> do
    enemy <- getEnemy eid
    n <- keywordValue (\case Toughness x -> Just x; _ -> Nothing) (EnemyTarget eid) (toCardDef enemy)
    pure (eid, n)
  let highest = maximumMay (map snd values)
  filterM
    ( \(pid, eid) -> do
        mods <- getModifiers eid
        pure $ CannotEngagePlayer pid `notElem` mods
          && CannotLeaveStagingArea `notElem` mods
          && all (== pid) [p | EngagesOnly p <- mods]
    )
    [(pid, eid) | (eid, n) <- values, Just n == highest, pid <- players]
