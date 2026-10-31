module LordOfTheRings.Quest.Cards.GatherYourCourage (gatherYourCourage) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Card.PlayerCard (PlayerCard (pcOwner))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype GatherYourCourage = GatherYourCourage QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge.
gatherYourCourage :: QuestCard GatherYourCourage
gatherYourCourage = quest GatherYourCourage Cards.gatherYourCourage

-- | When Revealed: Each player places the top 8 cards of his deck facedown under this
-- quest.
-- Forced: When this quest is defeated, shuffle each card under this quest into its owner's
-- deck.
instance RunMessage GatherYourCourage where
  runMessage msg q@(GatherYourCourage attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        top <- take 8 . (.deck) <$> getPlayer p
        for_ top \c -> removeFromDeck p c.id
        unless (null top) $ placeUnderneath attrs top
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      let owned = [(owner, PlayerCard pc) | PlayerCard pc <- attrs.underneath, Just owner <- [pcOwner pc]]
      for_ owned \(owner, c) -> putOnTopOfDeck owner c
      traverse_ shufflePlayerDeck (ordNub $ map fst owned)
      GatherYourCourage <$> runMessage msg attrs
    _ -> GatherYourCourage <$> runMessage msg attrs
