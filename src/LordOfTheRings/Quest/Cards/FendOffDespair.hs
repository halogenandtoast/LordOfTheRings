module LordOfTheRings.Quest.Cards.FendOffDespair (fendOffDespair) where

import LordOfTheRings.Campaign (CampaignLogUpdate (AdjustThreatPenalty))
import LordOfTheRings.Card (CardType (EventType), cdCardType, toCardDef)
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FendOffDespair = FendOffDespair QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest of the Angmar Awakened campaign, a burden shuffled into the
-- encounter deck from The Wastes of Eriador on.
fendOffDespair :: QuestCard FendOffDespair
fendOffDespair = quest FendOffDespair Cards.fendOffDespair

-- | Each event card gains doomed 1: each in a player's hand, as it is played from there.
instance HasModifiersFor FendOffDespair where
  getModifiersFor (FendOffDespair a) = do
    players <- getRemainingPlayers
    for_ players \p -> do
      hand <- (.hand) <$> getPlayer p
      for_ [c | c <- hand, cdCardType (toCardDef c) == EventType] \c ->
        modified a (CardIdTarget c.id) [GainKeyword (Doomed 1)]

-- | Forced: When Fend Off Despair is defeated, mark in the Campaign Log that each player's
-- starting threat is reduced by 2. (This reduction is cumulative.) It comes off the threat
-- penalty every player starts with.
instance RunMessage FendOffDespair where
  runMessage msg (FendOffDespair attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      push $ UpdateCampaignLog (AdjustThreatPenalty (-2))
      FendOffDespair <$> runMessage msg attrs
    _ -> FendOffDespair <$> runMessage msg attrs
