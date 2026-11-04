module LordOfTheRings.Quest.Cards.LootTheDungeons (lootTheDungeons) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Card (toCardCode)
import Data.List (elemIndex)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype LootTheDungeons = LootTheDungeons QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest of the Angmar Awakened campaign, in Escape from Mount Gram.
lootTheDungeons :: QuestCard LootTheDungeons
lootTheDungeons = quest LootTheDungeons Cards.lootTheDungeons

-- | Loot the Dungeons is considered to be in each player's staging area and gets +4 quest
-- points for each player in the game at every staging area.
instance HasModifiersFor LootTheDungeons where
  getModifiersFor (LootTheDungeons a) = do
    players <- length <$> getPlayerOrder
    modified a a [QuestPointModifier (4 * players)]

{- | Forced: When Loot the Dungeons is defeated, each player (at every staging area) may
choose one of the following boons and add it to their hand: Berelind, Chieftain's Brooch,
Coruhaer, Laermuin, Mail of Eärnur, or Silverthorn. Add the chosen cards to the Campaign
Pool, for the decks of the players who chose them.
The players choose in turn order, each boon taken once (the box has 1 of each).
-}
instance RunMessage LootTheDungeons where
  runMessage msg (LootTheDungeons attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      order <- getsGame (.playerOrder)
      eachPlayerMayTakeOneOf
        "lootTheDungeons.chooseBoon"
        (Just $ toTarget attrs)
        [Allies.berelind, Attachments.chieftainsBrooch, Allies.coruhaer, Allies.laermuin, Attachments.mailOfEarnur, Attachments.silverthorn]
        \p boon -> do
          putInHand boon
          updateCampaignLog $ AddToPool $ PoolCard (toCardCode boon) (elemIndex p order) Nothing
      LootTheDungeons <$> runMessage msg attrs
    _ -> LootTheDungeons <$> runMessage msg attrs
