-- | What the cards of The Siege of Cair Andros share: its Battleground locations, which
-- take damage, and the Besiegers' shadows on them.
module LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (
  lowestThreatBattleground,
  besiegerShadow,
  runBattleground,
  runNightmareBattleground,
  chooseBattleground,
) where

import LordOfTheRings.Card (CardDef, cdCardCode, cdQuestPoints, toCardDef)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types ()
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
  -- the Response is the first player's to take, if the stage is still in the quest deck
  AddToVictoryDisplay (isTarget attrs -> True) -> do
    able <- getsGame (any ((== cdCardCode stage) . toCardCode) . (.questDeck) . toAttrs . gameScenario)
    when able do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "battleground.removeStage"
        causedBy attrs
        labeled "battleground.remove" $ removeFromQuestDeck stage
        labeled "battleground.keep" $ pure ()
    runMessage msg attrs
  _ -> runMessage msg attrs

{- | A nightmare Battleground: "If it has as much damage as its quest points, discard it
and" the effect follows.
-}
runNightmareBattleground
  :: QueueT Message GameT () -> Message -> LocationAttrs -> QueueT Message GameT LocationAttrs
runNightmareBattleground effect msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Damage _ -> do
    attrs' <- runMessage msg attrs
    when (attrs'.damage >= fromMaybe 0 (cdQuestPoints $ toCardDef attrs)) do
      discard attrs
      effect
    pure attrs'
  _ -> runMessage msg attrs

-- | The player chooses a Battleground location in play for the effect, if there is one.
chooseBattleground
  :: (ReverseQueue m, Targetable source)
  => source -> PlayerId -> (LocationId -> QueueT Message m ()) -> m ()
chooseBattleground source pid effect = do
  battlegrounds <- select (LocationWithTrait Battleground)
  chooseOrRunOneM pid do
    questionLabeled "theSiegeOfCairAndrosNightmare.battleground"
    causedBy source
    targets battlegrounds effect
