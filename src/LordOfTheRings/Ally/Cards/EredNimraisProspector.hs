module LordOfTheRings.Ally.Cards.EredNimraisProspector (eredNimraisProspector) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))

newtype EredNimraisProspector = EredNimraisProspector AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eredNimraisProspector :: AllyCard EredNimraisProspector
eredNimraisProspector = ally EredNimraisProspector Cards.eredNimraisProspector

-- | Response: After Ered Nimrais Prospector enters play, discard the top 3 cards of your
-- deck. Then, choose and shuffle 1 card from your discard pile back into your deck.
instance HasAbilities EredNimraisProspector where
  getAbilities (EredNimraisProspector a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage EredNimraisProspector where
  runMessage msg s@(EredNimraisProspector a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discardTopOfDeck pid 3
      -- the discard pile is looked at once the cards, and what they trigger, are there
      push $ DoStep 1 msg
      pure s
    DoStep 1 (UseCardAbility pid (isSource a -> True) 1 _) -> do
      discarded <- (.discard) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "eredNimraisProspector.shuffle"
        causedBy a
        showing discarded
        targets [pc | PlayerCard pc <- discarded] \pc -> shuffleFromDiscardIntoDeck pid pc.id
      pure s
    _ -> EredNimraisProspector <$> runMessage msg a
