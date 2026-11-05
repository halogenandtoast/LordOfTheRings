{- | The Ered Mithrin campaign's treasures as its cards reach them: those undiscovered, and
"look at 3 random undiscovered treasures and attach 1 of them to ... as a guarded
objective" (MEC115 rules, "Treasure").
-}
module LordOfTheRings.Helpers.Treasure (
  undiscoveredTreasures,
  guardUndiscoveredTreasure,
  treasuresAttachedTo,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (campaignLogPool), PoolCard (..))
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gamePlayers, gameScenario)
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Id (CardId, PlayerId)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (GuardRelease (AttachWhenFree), Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioGuardedPlayerCards))
import LordOfTheRings.Target

{- | The treasures not yet discovered: none written in the campaign log, nor in the game
now, claimed, guarded or in a deck, so as not to be found twice.
-}
undiscoveredTreasures :: HasGame m => m [CardDef]
undiscoveredTreasures = do
  logged <- maybe [] (map poolCardCode . campaignLogPool) <$> getCampaignLog
  attached <- getsGame (map toCardCode . toList . (.entities.attachments))
  held <- getsGame (concatMap (\p -> map toCardCode (p.hand <> p.deck <> p.discard)) . toList . gamePlayers)
  guarded <- getsGame (map (toCardCode . snd) . concat . Map.elems . scenarioGuardedPlayerCards . toAttrs . gameScenario)
  let found = logged <> attached <> held <> guarded
  pure [d | d <- Attachments.eredMithrinTreasures, toCardCode d `notElem` found]

{- | The player looks at 3 random undiscovered treasures and attaches 1 of them to the card
as a guarded objective: free of encounters, the players claim it and attach it to an
eligible character ('AttachWhenFree').
-}
guardUndiscoveredTreasure :: (ReverseQueue m, Targetable source) => PlayerId -> source -> CardId -> m ()
guardUndiscoveredTreasure pid source guard' = do
  defs <- take 3 <$> (shuffleM =<< undiscoveredTreasures)
  cards <- traverse (`genPlayerCard` Just pid) defs
  chooseOneM pid do
    questionLabeled "treasure.guard"
    causedBy source
    showing (map PlayerCard cards)
    for_ cards \c -> targeting (CardIdTarget c.id) $ guardPlayerCard guard' AttachWhenFree c

-- | The treasures in play attached to the target.
treasuresAttachedTo :: HasGame m => Target -> m [Target]
treasuresAttachedTo host = getsGame \g ->
  [toTarget a | a <- toList g.entities.attachments, isTreasure a, (toAttrs a).placement == AttachedTo host]
