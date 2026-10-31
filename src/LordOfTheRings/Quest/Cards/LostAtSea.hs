module LordOfTheRings.Quest.Cards.LostAtSea (lostAtSea) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype LostAtSea = LostAtSea QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

lostAtSea :: QuestCard LostAtSea
lostAtSea = quest LostAtSea Cards.lostAtSea

-- | Sailing. Cancel the first sailing success icon found during each Sailing test.
instance HasModifiersFor LostAtSea where
  getModifiersFor (LostAtSea a) = when a.flipped $ modified a GameTarget [CancelFirstSailingIcon]

-- | Forced: At the end of the quest phase, if your heading is on-course, this stage is
-- defeated.
instance HasAbilities LostAtSea where
  getAbilities (LostAtSea a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

-- | When Revealed: Set your heading to the worst setting.
-- When this stage is defeated, look at the top 3 stages of the quest deck and advance to
-- one of your choice, placing the others on the bottom of the quest deck: it goes to the
-- victory display itself, in place of the scenario's advance.
instance RunMessage LostAtSea where
  runMessage msg q@(LostAtSea attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      LostAtSea <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      setHeadingToWorst attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM isOnCourse do
        addToVictoryDisplay attrs
        top <- take 3 <$> getsGame ((.questDeck) . toAttrs . (.scenario))
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "voyageAcrossBelegaer.chooseStage"
          showing (map EncounterCard top)
          targets top \chosen -> do
            for_ (filter (/= chosen) top) (putOnBottomOfQuestDeck . EncounterCard)
            advanceToQuest chosen
      pure q
    _ -> LostAtSea <$> runMessage msg attrs
