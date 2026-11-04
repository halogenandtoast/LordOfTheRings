module LordOfTheRings.Enemy.Cards.CunningInformant (cunningInformant) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (scrollHolder)

newtype CunningInformant = CunningInformant EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cunningInformant :: EnemyCard CunningInformant
cunningInformant = enemy CunningInformant Cards.cunningInformant

-- | While Alcaron's Scroll is attached to a hero, Cunning Informant gets +3 attack.
-- During the encounter phase, if Alcaron's Scroll is attached to a hero, that hero's
-- controller is considered to have +10 threat for the purpose of engagement checks. Only the
-- first copy in play gives it, as it does not stack.
instance HasModifiersFor CunningInformant where
  getModifiersFor (CunningInformant a) =
    scrollHolder >>= traverse_ \holder -> do
      modifySelf a [AttackModifier 3]
      encounter <- getsGame ((== EncounterPhase) . gamePhase)
      firstCopy <- (== Just a.id) . minimumMay <$> select (EnemyWithCardCode $ toCardCode Cards.cunningInformant)
      when (encounter && firstCopy) do
        modified a (PlayerTarget holder) [ThreatForEngagementChecks 10]

instance RunMessage CunningInformant where
  runMessage msg (CunningInformant attrs) = CunningInformant <$> runMessage msg attrs
