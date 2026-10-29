module LordOfTheRings.Treachery.Cards.InNeedOfRest (inNeedOfRest) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InNeedOfRest = InNeedOfRest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

inNeedOfRest :: TreacheryCard InNeedOfRest
inNeedOfRest = treachery InNeedOfRest Cards.inNeedOfRest

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Forced: After a time
-- counter is removed from the current quest, attached hero takes 1 damage."
instance HasAbilities InNeedOfRest where
  getAbilities (InNeedOfRest a) = [mkAbility a 1 $ forced $ TimeCountersRemovedFromQuestWindow After | AttachedTo _ <- [a.placement]]

-- | When Revealed: Remove a hero from the quest and attach In Need of Rest to that hero.
instance RunMessage InNeedOfRest where
  runMessage msg t@(InNeedOfRest attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) =<< select AnyHero
      questing <- filterM (\h -> selectAny (HeroWithId h <> CommittedHero)) free
      lead <- getLead
      -- a hero on the quest is removed from it; with none, it still attaches to a hero, as
      -- much of the effect as can be resolved
      chooseOrRunOneM lead do
        questionLabeled "inNeedOfRest.attach"
        causedBy attrs
        targets (if null questing then free else questing) \h -> do
          when (h `elem` questing) $ removeFromQuest h
          attachTreachery attrs.treacheryId h
      pure t
    -- a time counter at a time, each its 1 damage
    UseCardAbility _ (isSource attrs -> True) 1 windows | AttachedTo host <- attrs.placement -> do
      dealDamage attrs host $ sum [n | Window _ (TimeCountersRemoved (QuestTarget _) n) <- windows]
      pure t
    _ -> InNeedOfRest <$> runMessage msg attrs
