module LordOfTheRings.Treachery.Cards.PressingNeeds (pressingNeeds) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EncounterSideQuestType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PressingNeeds = PressingNeeds TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pressingNeeds :: TreacheryCard PressingNeeds
pressingNeeds = treachery PressingNeeds Cards.pressingNeeds

-- | Doomed 2. When Revealed: Either search the encounter deck and discard pile for an
-- encounter side quest and reveal it, or choose a different quest card in play to be the
-- current quest until the end of the phase. Shuffle the encounter deck.
-- The first player chooses; an option with nothing to find or choose is not offered.
instance RunMessage PressingNeeds where
  runMessage msg t@(PressingNeeds attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let sideQuest = (== EncounterSideQuestType) . cdCardType . toCardDef
          inDeck = filter sideQuest scenario.encounterDeck
          inDiscard = filter sideQuest scenario.encounterDiscard
      quests <- questCardsInPlay
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "pressingNeeds.choose"
        causedBy attrs
        unless (null inDeck && null inDiscard) $ labeled "pressingNeeds.search" $ chooseOneM lead do
          questionLabeled "pressingNeeds.chooseSideQuest"
          causedBy attrs
          showing $ map EncounterCard $ inDeck <> inDiscard
          targets inDeck \c -> do
            removeFromEncounterDeck c
            push $ RevealCard c
          targets inDiscard revealFromEncounterDiscard
        when (quests > 1) $ labeled "pressingNeeds.currentQuest" $ chooseNewCurrentQuest lead attrs
      shuffleEncounterDeck
      pure t
    _ -> PressingNeeds <$> runMessage msg attrs
