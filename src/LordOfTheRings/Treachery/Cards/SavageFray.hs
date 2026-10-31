module LordOfTheRings.Treachery.Cards.SavageFray (savageFray) where

import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SavageFray = SavageFray TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageFray :: TreacheryCard SavageFray
savageFray = treachery SavageFray Cards.savageFray

-- | Doomed 2.
-- When Revealed: If the current quest has a keyword, until the end of the round, switch
-- that keyword to battle or siege, or siege to battle, if able. If the current quest does
-- not have a keyword, Savage Fray gains surge.
instance RunMessage SavageFray where
  runMessage msg t@(SavageFray attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      keyword <- currentQuestHasAKeyword
      battle <- currentQuestHas Battle
      siege <- currentQuestHas Siege
      getCurrentQuest >>= \case
        Just q | keyword ->
          if
            | battle -> roundModifiers attrs q [LoseKeyword Battle, GainKeyword Siege]
            | siege -> roundModifiers attrs q [LoseKeyword Siege, GainKeyword Battle]
            -- ponytail: another keyword (archery, time) is kept; the quest gains the one
            -- the first player picks
            | otherwise -> do
                lead <- getLead
                chooseOneM lead do
                  questionLabeled "savageFray.choose"
                  causedBy attrs
                  labeled "savageFray.battle" $ roundModifier attrs q (GainKeyword Battle)
                  labeled "savageFray.siege" $ roundModifier attrs q (GainKeyword Siege)
        _ -> revealEncounterCard
      pure t
    _ -> SavageFray <$> runMessage msg attrs
