module LordOfTheRings.Quest.Cards.HastyCouncil (hastyCouncil) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype HastyCouncil = HastyCouncil QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hastyCouncil :: QuestCard HastyCouncil
hastyCouncil = quest HastyCouncil Cards.hastyCouncil

instance HasAbilities HastyCouncil where
  getAbilities (HastyCouncil a) = searchForAnExit a

-- | When Revealed: Shuffle all copies of A Foe Beyond from the encounter discard pile back
-- into the encounter deck. Reveal the top 2 cards of the quest deck. Choose 1 to become the
-- active quest (flipped to side 2B) and put the other on the bottom of the quest deck.
-- Then, add Hasty Council to your victory display.
--
-- Hasty Council leaves first, so the chosen stage is the only one in play as it resolves.
instance RunMessage HastyCouncil where
  runMessage msg (HastyCouncil attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      shuffleEncounterDiscardIntoDeck (cardIs Treacheries.aFoeBeyond)
      top <- getsGame (take 2 . (.questDeck) . toAttrs . (.scenario))
      if null top
        then addToVictoryDisplay attrs >> advanceQuest
        else do
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled "hastyCouncil.choose"
            causedBy attrs
            showing (map EncounterCard top)
            targets top \chosen -> do
              for_ (filter (/= chosen) top) (putOnBottomOfQuestDeck . EncounterCard)
              addToVictoryDisplay attrs
              putQuestIntoPlayFlipped chosen
      HastyCouncil <$> runSearchForAnExit msg attrs
    _ -> HastyCouncil <$> runSearchForAnExit msg attrs
