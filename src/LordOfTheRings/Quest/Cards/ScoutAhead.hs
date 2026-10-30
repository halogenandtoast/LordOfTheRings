module LordOfTheRings.Quest.Cards.ScoutAhead (scoutAhead) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (ObjectiveAllyType, ObjectiveType), cdCardType, cdVictoryPoints, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ScoutAhead = ScoutAhead QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 per deck, as its card def says.
scoutAhead :: QuestCard ScoutAhead
scoutAhead = quest ScoutAhead Cards.scoutAhead

-- | Response: When this stage is defeated, the first player searches the top X cards of the
-- encounter deck for 1 non-objective card worth no victory points and adds it to the
-- victory display. Put the remaining cards back in any order. X is the number of players
-- in the game plus 4.
instance RunMessage ScoutAhead where
  runMessage msg (ScoutAhead attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      lead <- getLead
      x <- (+ 4) . length <$> getRemainingPlayers
      top <- take x . (.encounterDeck) . toAttrs <$> getsGame (.scenario)
      let eligible c =
            cdCardType (toCardDef c) `notElem` [ObjectiveType, ObjectiveAllyType]
              && maybe True (== 0) (cdVictoryPoints $ toCardDef c)
      chooseOneM lead do
        questionLabeled "scoutAhead.search"
        causedBy attrs
        showing (map EncounterCard top)
        targets (filter eligible top) \c -> do
          addToVictoryDisplay c
          -- the rest go back on top, the last chosen on top
          chooseOneAtATimeM lead do
            questionLabeled "putOnTopOfEncounterDeck"
            causedBy attrs
            showing (map EncounterCard $ filter (/= c) top)
            targets (filter (/= c) top) \r -> do
              removeFromEncounterDeck r
              putOnTopOfEncounterDeck r
      ScoutAhead <$> runMessage msg attrs
    _ -> ScoutAhead <$> runMessage msg attrs
