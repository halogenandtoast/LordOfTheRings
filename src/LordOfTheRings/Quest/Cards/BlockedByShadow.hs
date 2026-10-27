module LordOfTheRings.Quest.Cards.BlockedByShadow (blockedByShadow) where

import LordOfTheRings.Card (CardType (TreacheryType), toCardDef)
import LordOfTheRings.Card.CardDef (CardDef (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype BlockedByShadow = BlockedByShadow QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blockedByShadow :: QuestCard BlockedByShadow
blockedByShadow = quest BlockedByShadow Cards.blockedByShadow

instance HasAbilities BlockedByShadow where
  getAbilities (BlockedByShadow a) = searchForAnExit a <> bypass a

-- | When Revealed: The first player chooses 1 of the following:
-- * Each player discards 1 card from the top of the encounter deck. If the card is a
--   treachery card, the discarding player is eliminated from the game.
-- * Reveal the next quest card, putting Blocked by Shadow on the bottom of the quest deck.
-- If the players defeat this stage, they escape and win the game.
instance RunMessage BlockedByShadow where
  runMessage msg q@(BlockedByShadow attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "blockedByShadow.choose"
        causedBy attrs
        labeled "blockedByShadow.discard" $ eachPlayer \p -> discardTopOfEncounterDeckFor attrs p 1
        labeled "blockedByShadow.nextQuest" do
          putOnBottomOfQuestDeck attrs
          advanceQuest
      BlockedByShadow <$> runSearchForAnExit msg attrs
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget p) cards -> do
      when (any ((== TreacheryType) . cdCardType . toCardDef) cards) $ eliminatePlayer p
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped && attrs.progress + n >= attrs.questPoints -> do
      winGame
      pure q
    _ -> BlockedByShadow <$> runSearchForAnExit msg attrs
