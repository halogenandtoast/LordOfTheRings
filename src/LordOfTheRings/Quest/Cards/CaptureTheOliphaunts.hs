module LordOfTheRings.Quest.Cards.CaptureTheOliphaunts (captureTheOliphaunts) where

import LordOfTheRings.Card (Card (EncounterCard), cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (captureObjectives, capturedMumak, wildMumak)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (CaptureTrait))

newtype CaptureTheOliphaunts = CaptureTheOliphaunts QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

captureTheOliphaunts :: QuestCard CaptureTheOliphaunts
captureTheOliphaunts = quest CaptureTheOliphaunts Cards.captureTheOliphaunts

-- | This stage cannot be defeated while there are any Capture objectives in play.
instance HasModifiersFor CaptureTheOliphaunts where
  getModifiersFor (CaptureTheOliphaunts a) = when a.flipped do
    held <- notNull <$> captureObjectives
    when held $ modifySelf a [CannotBeDefeated]

-- | Forced: At the beginning of the quest phase, if there is no active location, reveal an
-- additional encounter card this phase.
instance HasAbilities CaptureTheOliphaunts where
  getAbilities (CaptureTheOliphaunts a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

{- | When Revealed: Starting with the first player, each player attaches 1 random set-aside
Capture objective to a Wild Mûmak in play: one with none attached yet while there is one.
If the players defeat this stage, they win the game.
-}
instance RunMessage CaptureTheOliphaunts where
  runMessage msg q@(CaptureTheOliphaunts attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      CaptureTheOliphaunts <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      aside <- getsGame (filter ((CaptureTrait `member`) . cdCardTraits . toCardDef) . scenarioSetAside . toAttrs . (.scenario))
      picked <- take 1 <$> shuffleM aside
      case (drop (n - 1) players, picked) of
        (pid : _, [card]) -> do
          mumaks <- select wildMumak
          held <- mapMaybe capturedMumak <$> captureObjectives
          let free = filter (`notElem` held) mumaks
          chooseOrRunOneM pid do
            questionLabeled "captureTheOliphaunts.attach"
            causedBy attrs
            showing [EncounterCard card]
            targets (if null free then mumaks else free) $ putObjectiveIntoPlayAttached card
          doStep (n + 1) (WhenRevealed $ toTarget attrs)
        _ -> pure ()
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      active <- selectAny IsActiveLocation
      unless active $ phaseModifier attrs GameTarget (AdditionalStagingReveals 1)
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> CaptureTheOliphaunts <$> runMessage msg attrs
