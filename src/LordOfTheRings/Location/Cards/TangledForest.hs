module LordOfTheRings.Location.Cards.TangledForest (tangledForest) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype TangledForest = TangledForest LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tangledForest :: LocationCard TangledForest
tangledForest = location TangledForest Cards.tangledForest

{- | While Tangled Forest is in the staging area, it gains: "Forced: After any amount of
progress is removed from the current quest, assign X damage among characters in play. X is
the number of progress just removed. This ability does not stack with other copies of
Tangled Forest." Only the copy in the staging area that came first answers; the first
player assigns the damage.
-}
instance RunMessage TangledForest where
  runMessage msg (TangledForest a) = case msg of
    RemoveTokens _ target@(QuestTarget qid) Progress n | n > 0, a.placement == StagingArea -> do
      current <- getCurrentQuest
      copies <- select (LocationWithCardCode (toCardCode a) <> LocationInStagingArea)
      progress <- getsGame (maybe 0 (.progress) . lookup qid . (.entities.quests))
      let removed = min n progress
      when (current == Just target && minimumMay copies == Just a.id && removed > 0) do
        lead <- getLead
        characters <- selectCharacters AnyHero AnyAlly
        dealDamageDividedAmong a lead characters removed
      TangledForest <$> runMessage msg a
    _ -> TangledForest <$> runMessage msg a
