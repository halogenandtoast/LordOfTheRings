module LordOfTheRings.Quest.Cards.TheWitchKingsGift (theWitchKingsGift) where

import LordOfTheRings.Card (Card (PlayerCard), EncounterCard (..), toCard, toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheWitchKingsGift = TheWitchKingsGift QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Time 5.
theWitchKingsGift :: QuestCard TheWitchKingsGift
theWitchKingsGift = quest TheWitchKingsGift Cards.theWitchKingsGift

-- | Forced: When the last time counter is removed from this quest, reanimate each card in
-- each player's discard pile. Then, remove The Witch-king's Gift from the game.
instance HasAbilities TheWitchKingsGift where
  getAbilities (TheWitchKingsGift a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When this quest is defeated, place it in the encounter discard pile: it leaves the
-- victory display it goes to as any side quest.
instance RunMessage TheWitchKingsGift where
  runMessage msg q@(TheWitchKingsGift attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        player <- getPlayer p
        for_ [c | PlayerCard c <- player.discard] \c -> void $ reanimate c.id
      removeFromGame attrs
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      removeFromVictoryDisplay (toCard attrs)
      push $ AddToEncounterDiscard [MkEncounterCard attrs.cardId (toCardCode attrs)]
      TheWitchKingsGift <$> runMessage msg attrs
    _ -> TheWitchKingsGift <$> runMessage msg attrs
