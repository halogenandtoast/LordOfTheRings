module LordOfTheRings.Treachery.Cards.BarrenAndDesolate (barrenAndDesolate) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BarrenAndDesolate = BarrenAndDesolate TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

barrenAndDesolate :: TreacheryCard BarrenAndDesolate
barrenAndDesolate = treachery BarrenAndDesolate Cards.barrenAndDesolate

-- | Counts as a Condition attachment with the text: "Limit 1 per quest. While attached quest
-- is the current quest, each character gets -1 willpower."
instance HasModifiersFor BarrenAndDesolate where
  getModifiersFor (BarrenAndDesolate a) = case a.placement of
    AttachedTo host -> do
      current <- getCurrentQuest
      when (current == Just host) do
        characters <- selectCharacters AnyHero AnyAlly
        for_ characters \c -> modified a c [WillpowerModifier (-1)]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to a side quest. The first player chooses one without a copy.
instance RunMessage BarrenAndDesolate where
  runMessage msg t@(BarrenAndDesolate attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      sideQuests <- getEncounterSideQuests
      free <- filterM (fmap null . treacheriesOn (toCardCode attrs)) sideQuests
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "barrenAndDesolate.attach"
        causedBy attrs
        targets free (attachTreachery attrs.treacheryId)
      pure t
    _ -> BarrenAndDesolate <$> runMessage msg attrs
