module LordOfTheRings.Ally.Cards.Firyal (firyal) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard))

newtype Firyal = Firyal AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

firyal :: AllyCard Firyal
firyal = ally Firyal Cards.firyal

-- | Response: After Firyal commits to the quest, look at the top card of the encounter
-- deck. Then, you may discard the looked at card.
instance HasAbilities Firyal where
  getAbilities (Firyal a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage Firyal where
  runMessage msg s@(Firyal a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- getTopOfEncounterDeck
      for_ top \card -> chooseOneM pid do
        questionLabeled "topOfEncounterDeck"
        causedBy a
        showing [EncounterCard card]
        labeled "leaveOnTop" $ pure ()
        labeled "firyal.discard" $ discardTopOfEncounterDeck a 1
      pure s
    _ -> Firyal <$> runMessage msg a
