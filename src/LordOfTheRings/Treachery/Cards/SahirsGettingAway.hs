module LordOfTheRings.Treachery.Cards.SahirsGettingAway (sahirsGettingAway) where

import LordOfTheRings.Placement (Placement (StormcallerArea))
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (placeStormcallerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SahirsGettingAway = SahirsGettingAway TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsGettingAway :: TreacheryCard SahirsGettingAway
sahirsGettingAway = treachery SahirsGettingAway Cards.sahirsGettingAway

-- | When Revealed: For each Ship enemy in the Stormcaller's area, place 1 progress on the
-- Stormcaller's quest stage. Then, either shift your heading off-course or add 8 threat to
-- the staging area until the end of the phase.
-- Shadow: If this attack damages a Ship objective, place 1 progress on the Stormcaller's
-- quest stage.
-- ponytail: a Ship objective defending, or the attack undefended by a Ship enemy, counts as
-- damaging one, whatever the damage comes to
instance RunMessage SahirsGettingAway where
  runMessage msg t@(SahirsGettingAway attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ships <- traverse getEnemy =<< select shipEnemy
      placeStormcallerProgress $ length $ filter ((== StormcallerArea) . (.placement)) ships
      lead <- getLead
      chooseOneM lead do
        questionLabeled "sahirsGettingAway.choose"
        causedBy attrs
        labeled "sahirsGettingAway.offCourse" $ shiftHeadingOffCourse attrs
        labeled "sahirsGettingAway.threat" $ phaseModifier attrs GameTarget (StagingThreatBonus 8)
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      shipDefends <- or <$> for [aid | AllyTarget aid <- defenders] \aid -> selectAny (shipObjective <> AllyWithId aid)
      shipAttacks <- selectAny (shipEnemy <> EnemyWithId eid)
      when (shipDefends || (null defenders && shipAttacks)) $ placeStormcallerProgress 1
      pure t
    _ -> SahirsGettingAway <$> runMessage msg attrs
