module LordOfTheRings.Location.Cards.DarkPathway (darkPathway) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardDef (cdKeywords), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (canRemoveQuestTimeCounter)

newtype DarkPathway = DarkPathway LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPathway :: LocationCard DarkPathway
darkPathway = location DarkPathway Cards.darkPathway

-- | While Dark Pathway is the active location, the first treachery card without the
-- printed surge keyword revealed during the quest phase gains surge. Read off the window
-- after it is revealed, its surge revealing the next card; 'FirstEffectUsed' marks it done.
-- Shadow: Either remove 1 time counter from the current quest or treat this attack as
-- undefended.
instance RunMessage DarkPathway where
  runMessage msg l@(DarkPathway a) = case msg of
    CheckWindows ws | a.placement == ActiveLocation -> do
      phase <- getsGame gamePhase
      used <- elem FirstEffectUsed <$> getModifiers a
      inPlay <- getsGame (.entities.treacheries)
      let revealed = mapMaybe (`Map.lookup` inPlay) [tid | Window After (Revealed (TreacheryTarget tid)) <- ws]
      when (phase == QuestPhase && not used && any ((Surge `notMember`) . cdKeywords . toCardDef) revealed) do
        phaseModifier a a FirstEffectUsed
        revealEncounterCard
      DarkPathway <$> runMessage msg a
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      removable <- canRemoveQuestTimeCounter
      chooseOrRunOneM pid do
        questionLabeled "darkPathway.choose"
        causedBy a
        when removable $ labeled "darkPathway.removeTimeCounter" $ removeTimeCountersFromCurrentQuest a 1
        unless (null defenders) $ labeled "darkPathway.undefended" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> DarkPathway <$> runMessage msg a
