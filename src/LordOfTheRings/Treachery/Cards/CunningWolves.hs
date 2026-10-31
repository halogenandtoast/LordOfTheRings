module LordOfTheRings.Treachery.Cards.CunningWolves (cunningWolves) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)
import LordOfTheRings.Trait (Trait (Warg))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CunningWolves = CunningWolves TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cunningWolves :: TreacheryCard CunningWolves
cunningWolves = treachery CunningWolves Cards.cunningWolves

-- | Counts as a Condition attachment with the text: "Attached quest gets +3 quest points.
-- Each Warg enemy cannot be optionally engaged."
instance HasModifiersFor CunningWolves where
  getModifiersFor (CunningWolves a) = case a.placement of
    AttachedTo host -> do
      modified a host [QuestPointModifier 3]
      wargs <- select (EnemyWithTrait Warg)
      for_ wargs \e -> modified a e [CannotBeOptionallyEngagedBy AnyPlayer]
    _ -> pure ()

-- | When Revealed: Attach to the current quest.
-- Shadow: If it is Night, deal attacking enemy 2 additional shadow cards.
instance RunMessage CunningWolves where
  runMessage msg t@(CunningWolves attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      whenM isNight $ dealShadowCardsDuringAttack eid pid defenders 2
      pure t
    _ -> CunningWolves <$> runMessage msg attrs
