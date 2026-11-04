module LordOfTheRings.Attachment.Cards.TheRoadGoesEverOn (theRoadGoesEverOn) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (PlayerSideQuestType))

newtype TheRoadGoesEverOn = TheRoadGoesEverOn AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a quest card in play.
theRoadGoesEverOn :: AttachmentCard TheRoadGoesEverOn
theRoadGoesEverOn = attachment TheRoadGoesEverOn Cards.theRoadGoesEverOn & attachTo AttachToQuest

-- | Response: When attached quest is defeated, the first player chooses a player. That
-- player searches his deck for a side quest, adds it to his hand, and shuffles his deck.
-- Answered as the quest is defeated, before its attachments are discarded, as The Long
-- Defeat is.
instance HasAbilities TheRoadGoesEverOn where
  getAbilities (TheRoadGoesEverOn a) =
    [mkAbility a 1 $ response $ QuestDefeatedWindow After host | AttachedTo host <- [a.placement]]

instance RunMessage TheRoadGoesEverOn where
  runMessage msg s@(TheRoadGoesEverOn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      choosePlayerM lead a \p -> searchDeck p a maxBound (CardWithType PlayerSideQuestType) False
      pure s
    _ -> TheRoadGoesEverOn <$> runMessage msg a
