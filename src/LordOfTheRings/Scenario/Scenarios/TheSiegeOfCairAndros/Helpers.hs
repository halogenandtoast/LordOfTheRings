-- | What the cards of The Siege of Cair Andros share: its Battleground locations, which
-- take damage, and the Besiegers' shadows on them.
module LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (
  lowestThreatBattleground,
  besiegerShadow,
  runBattleground,
) where

import LordOfTheRings.Card (CardDef, cdCardCode, cdQuestPoints, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Id
import LordOfTheRings.Location.Runner
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Damage))
import LordOfTheRings.Trait (Trait (Battleground))

-- | The Battleground locations in play with the lowest threat: one, or those tied, for the
-- players to choose among.
lowestThreatBattleground :: HasGame m => m [LocationId]
lowestThreatBattleground = do
  threats <- traverse (\l -> (,l) <$> getLocationThreat l) =<< select (LocationWithTrait Battleground)
  let lowest = foldr (min . fst) maxBound threats
  pure [l | (t, l) <- threats, t == lowest]

-- | "Shadow: Deal 2 damage to the location if it is in play. Otherwise, attacking enemy
-- gets +2 attack."
besiegerShadow :: (ReverseQueue m, Sourceable source) => source -> CardDef -> EnemyId -> m ()
besiegerShadow source target eid =
  select (LocationWithCardCode $ cdCardCode target) >>= \case
    lid : _ -> dealDamage source lid 2
    [] -> enemyGetsAttack source eid 2

{- | A Battleground: "If it has as much damage as its quest points, remove it from the game
(do not collect its victory points). Response: After it leaves play as an explored
location, remove the stage from the quest deck, if able."
-}
runBattleground :: CardDef -> Message -> LocationAttrs -> QueueT Message GameT LocationAttrs
runBattleground stage msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Damage _ -> do
    attrs' <- runMessage msg attrs
    when (attrs'.damage >= fromMaybe 0 (cdQuestPoints $ toCardDef attrs)) $ removeFromGame attrs
    pure attrs'
  -- ponytail: the Response always helps the players, so it is taken without asking
  AddToVictoryDisplay (isTarget attrs -> True) -> do
    removeFromQuestDeck stage
    runMessage msg attrs
  _ -> runMessage msg attrs
