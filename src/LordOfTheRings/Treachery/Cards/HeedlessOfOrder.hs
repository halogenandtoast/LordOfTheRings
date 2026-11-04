module LordOfTheRings.Treachery.Cards.HeedlessOfOrder (heedlessOfOrder) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HeedlessOfOrder = HeedlessOfOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, on its card def.
heedlessOfOrder :: TreacheryCard HeedlessOfOrder
heedlessOfOrder = treachery HeedlessOfOrder Cards.heedlessOfOrder

{- | When Revealed: Choose a different quest to be the current quest until the end of the
round, if able. If the current quest did not change as a result of this effect, this card
gains Doomed 3. The first player chooses; a stage of The Battle of Five Armies chosen
stays the current quest for the round (see 'setCurrentQuest').
-}
instance RunMessage HeedlessOfOrder where
  runMessage msg t@(HeedlessOfOrder attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      current <- getCurrentQuest
      others <- filterM (fmap (notElem CannotBeCurrentQuest) . getModifiers) . filter ((/= current) . Just) =<< getQuestCards
      lead <- getLead
      if null others then raiseThreatFromDoomed 3 else chooseNewCurrentQuest lead attrs
      pure t
    _ -> HeedlessOfOrder <$> runMessage msg attrs
