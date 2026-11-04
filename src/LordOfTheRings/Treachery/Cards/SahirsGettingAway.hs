module LordOfTheRings.Treachery.Cards.SahirsGettingAway (sahirsGettingAway) where

import LordOfTheRings.Card (CardType (ObjectiveAllyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Trait (Trait (Ship))
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
instance RunMessage SahirsGettingAway where
  runMessage msg t@(SahirsGettingAway attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeStormcallerProgress =<< selectCount (shipEnemy <> EnemyInStormcallerArea)
      lead <- getLead
      chooseOneM lead do
        questionLabeled "sahirsGettingAway.choose"
        causedBy attrs
        labeled "sahirsGettingAway.offCourse" $ shiftHeadingOffCourse attrs
        labeled "sahirsGettingAway.threat" $ phaseModifier attrs GameTarget (StagingThreatBonus 8)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      afterAttack eid [DoStep 1 msg]
      pure t
    -- read once the attack is over: a Ship objective it dealt damage to, still in play or
    -- destroyed by it
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      damaged <- attackDamaged eid
      destroyed <- attackDestroyed eid
      let ship d = Ship `member` cdCardTraits (toCardDef d.card) && cdCardType (toCardDef d.card) == ObjectiveAllyType
      hit <- flip anyM [aid | (AllyTarget aid, n) <- damaged, n > 0] \aid ->
        (||) <$> selectAny (shipObjective <> AllyWithId aid) <*> pure (any (\d -> d.target == AllyTarget aid && ship d) destroyed)
      when hit $ placeStormcallerProgress 1
      pure t
    _ -> SahirsGettingAway <$> runMessage msg attrs
